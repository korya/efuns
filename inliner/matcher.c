/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: matcher.c,v 1.2 2000/08/21 15:38:42 lefessan Exp $ */

#include <stdio.h>

#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

extern char *young_start, *young_ptr, *young_end, *heap_start, *heap_end;

#ifdef __alpha
typedef int page_table_entry;
#else
typedef char page_table_entry;
#endif

extern page_table_entry *page_table;

#define Is_young(val) \
  ((addr)(val) > (addr)young_start && (addr)(val) < (addr)young_end)

#define Page(p) ((unsigned long) (p) >> Page_log)
#define Is_in_heap(p) \
  ((addr)(p) >= (addr)heap_start && (addr)(p) < (addr)heap_end \
   && page_table [Page (p)])

/* Structural comparison on trees.
   May loop on cyclic structures. */

#define Matcher_tag 220

#define FIELD_REF  0
#define FIELD_TYPE 1
#define FIELD_VAL  2

#define TYPE_ALIAS Val_int(0)
#define TYPE_VAR   Val_int(1)

/* #define DEBUG_COMPARE */
#ifdef DEBUG_COMPARE
#define DCOMP(m)     fprintf(stderr,m)
#define DCOMP1(m,v1)     fprintf(stderr,m,v1)
#define DCOMP2(m,v1,v2)     fprintf(stderr,m,v1,v2)
#define DCOMP3(m,v1,v2,v3)     fprintf(stderr,m,v1,v2,v3)
#else
#define DCOMP(m)
#define DCOMP1(m,v1)
#define DCOMP2(m,v1,v2)
#define DCOMP3(m,v1,v2,v3)
#endif

value matcher_compare(value v1, value v2)
{
  tag_t t1, t2;
  
  tailcall:
  
  if (v1 == v2) { 
    return Val_true;}
  if (Is_long(v2)) {
    DCOMP("v2 is long\n");
    return Val_false;
  }
  /* If one of the objects is outside the heap (but is not an atom),
     use address comparison. Since both addresses are 2-aligned,
     shift lsb off to avoid overflow in subtraction. */
  if (!Is_atom(v2) && !Is_young(v2) && !Is_in_heap(v2))
    invalid_argument("matcher_compare: out of heap");
  t2 = Tag_val(v2);
  if(t2 == Matcher_tag) {
    DCOMP("VAR FOUND\n");
    modify(&Field(Field(v2, FIELD_REF),0), v1);
    if(Field(v2, FIELD_TYPE) == TYPE_ALIAS)
      return matcher_compare(v1, Field(v2, FIELD_VAL));
    else      { 
      
      return Val_true;
    }
  }
  
  if (Is_long(v1)) {
    DCOMP("v1 is long\n");
    return Val_false;
  }
  if (!Is_atom(v1) && !Is_young(v1) && !Is_in_heap(v1))
    invalid_argument("matcher_compare: v1 out of heap");
  t1 = Tag_val(v1);
  if (t1 != t2) {
    DCOMP("Different tags\n");
    return Val_false;
  }
  switch(t1) {
    case String_tag: {
      mlsize_t len1, len2, len;
      unsigned char * p1, * p2;
      len1 = string_length(v1);
      len2 = string_length(v2);
      
      if(len1 != len2) {
        DCOMP("Different string length\n");
        return Val_false;
      }
      
      for (len = len1,
          p1 = (unsigned char *) String_val(v1),
          p2 = (unsigned char *) String_val(v2);
          len > 0;
          len--, p1++, p2++){
        if (*p1 != *p2) {
          DCOMP("Different strings\n");
          return Val_false;
        }
      }
      return Val_true;
    }
    case Double_tag: {
      double d1 = Double_val(v1);
      double d2 = Double_val(v2);
      
      if (d1 == d2) return Val_true; else {
        DCOMP("Different doubles\n");
        return Val_false;
      }
    }
    case Double_array_tag: {
      mlsize_t sz1 = Wosize_val(v1) / Double_wosize;
      mlsize_t sz2 = Wosize_val(v2) / Double_wosize;
      mlsize_t i;
      
      if (sz1 != sz2) return Val_false;
      for (i = 0; i < sz1; i++) {
        double d1 = Double_field(v1, i);
        double d2 = Double_field(v2, i);
        if (d1 != d2) return Val_false;
      }
      return Val_true;
    }
    case Abstract_tag:
    case Custom_tag:
    invalid_argument("matcher_compare: abstract value");
    case Closure_tag:
    case Infix_tag:
    invalid_argument("matcher_compare: functional value");
    case Object_tag:
    if (Oid_val(v1) != Oid_val(v2)) return Val_false; else return Val_true;
    default: {
      mlsize_t sz1 = Wosize_val(v1);
      mlsize_t sz2 = Wosize_val(v2);
      value * p1, * p2;
      long res;
      int i;
      
      if (sz1 != sz2) {
        DCOMP2("Different tuple sizes %d %d\n", sz1, sz2);
        return Val_false;
      }
      if (sz1 == 0) {       
        return Val_true;
      }
      for(p1 = Op_val(v1), p2 = Op_val(v2), i=0;
          sz1 > 1;
          sz1--, p1++, p2++, i++) {
        DCOMP3("Testing tuple %lx %d/%d\n", v1, i, sz2);
        res = matcher_compare(*p1, *p2);
        if (res != Val_true) {
          DCOMP("Difference in tuple\n");
          return res;
        }
      }
      DCOMP3("Testing tuple %lx %d/%d\n", v1, i, sz2);
      v1 = *p1;
      v2 = *p2;
      goto tailcall;
    }
  }
}

#include "caml/alloc.h"

value matcher_make(value ref, value type) 
{
  value res;
 
  Begin_root(ref);
  res = alloc(3, Matcher_tag);
  initialize(&Field(res, FIELD_REF), ref);
  Field(res, FIELD_TYPE) = type;
  if (type == TYPE_VAR)
    Field(res, FIELD_VAL) = Val_unit;
  else {
    initialize(&Field(res, FIELD_VAL), Field(ref, 0));
  }
  End_roots();
  return res;
}

value matcher_copy(value v)
{
  tag_t tag;
  
  if (Is_long(v)) return v;
  /* If one of the objects is outside the heap (but is not an atom),
     use address comparison. Since both addresses are 2-aligned,
     shift lsb off to avoid overflow in subtraction. */
  if (!Is_atom(v) && !Is_young(v) && !Is_in_heap(v)){
    
    invalid_argument("matcher_copy: out of heap");
  }
  tag = Tag_val(v);
  if(tag == Matcher_tag) {
    
    if(Field(v, FIELD_TYPE) == TYPE_ALIAS)
      return matcher_copy(Field(v, FIELD_VAL));
    else {
      return Field(Field(v, FIELD_REF),0);
    }
  }
  switch(tag) {
    case String_tag: 
    case Double_array_tag: 
    case Object_tag:
    case Double_tag: 
    return v; /* No copy for strings, floats and objects */
    case Abstract_tag:
    invalid_argument("matcher_copy: abstract value");
    case Closure_tag:
    case Infix_tag:
    invalid_argument("matcher_copy: functional value");
    default: {
      mlsize_t size = Wosize_val(v);
      if(size > 0){
        value res = Val_unit;
        value copy;
        int i;
        
        Begin_roots2(res, v);
        res = alloc(size, tag);
        for(i = 0; i< size; i++) Field(res,i) = Val_unit;
        for(i = 0; i< size; i++) {        
          copy = matcher_copy(Field(v,i));
          modify(&Field(res,i), copy);
        }
        End_roots();
        return res;
      } else
        return Atom(0);
    }
  }
}
