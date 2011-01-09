/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"

/* Operations on arrays */

#include "caml/fail.h"
#include "caml/misc.h"

value array_get_addr(value array, value index) /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.get");
  return Field(array, idx);
}

value array_get_float(value array, value index) /* ML */
{
  long idx = Long_val(index);
  double d;
  value res;

  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    invalid_argument("Array.get");
  d = Double_field(array, idx);
#define Setup_for_gc
#define Restore_after_gc
  res = alloc(Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

value array_get(value array, value index)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_get_float(array, index);
  else
    return array_get_addr(array, index);
}

value array_set_addr(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array)) invalid_argument("Array.set");
  modify(&Field(array, idx), newval);
  return Val_unit;
}

value array_set_float(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  if (idx < 0 || idx >= Wosize_val(array) / Double_wosize)
    invalid_argument("Array.set");
  Store_double_field(array, idx, Double_val(newval));
  return Val_unit;
}

value array_set(value array, value index, value newval)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_set_float(array, index, newval);
  else
    return array_set_addr(array, index, newval);
}

value array_unsafe_get_float(value array, value index) /* ML */
{
  double d;
  value res;

  d = Double_field(array, Long_val(index));
#define Setup_for_gc
#define Restore_after_gc
  res = alloc( Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

value array_unsafe_get(value array, value index)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_unsafe_get_float(array, index);
  else
    return Field(array, Long_val(index));
}

value array_unsafe_set_addr(value array, value index, value newval)   /* ML */
{
  long idx = Long_val(index);
  modify(&Field(array, idx), newval);
  return Val_unit;
}

value array_unsafe_set_float(value array, value index, value newval)   /* ML */
{
  Store_double_field(array, Long_val(index), Double_val(newval));
  return Val_unit;
}

value array_unsafe_set(value array, value index, value newval)   /* ML */
{
  if (Tag_val(array) == Double_array_tag)
    return array_unsafe_set_float(array, index, newval);
  else
    return array_unsafe_set_addr(array, index, newval);
}
