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

/* $Id: fail.c,v 1.2 1999/09/20 13:49:03 lefessan Exp $ */

/* Raising exceptions from C. */

#include <signal.h>
#include <string.h>
#include <stdio.h>
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "stack.h"
#include "roots.h"

/* The globals holding predefined exceptions */

typedef char caml_generated_constant[256];
/* We claim these constants are big so that e.g. the Mips compiler
   will not assume that they are in the .sdata section */

extern caml_generated_constant Out_of_memory, Sys_error, Failure,
  Invalid_argument, End_of_file, Division_by_zero, Not_found, Match_failure;

/* Exception raising */

extern void raise_caml_exception (value bucket) Noreturn;

char * caml_exception_pointer = NULL;

void mlraise(value v)
{
#ifdef POSIX_SIGNALS
  sigset_t mask;
  sigemptyset(&mask);
  sigprocmask(SIG_SETMASK, &mask, NULL);
#else
#ifdef HAS_SIGSETMASK
  sigsetmask(0);
#endif
#endif
  Unlock_exn();
  if (caml_exception_pointer == NULL) fatal_uncaught_exception(v);

#ifndef Stack_grows_upwards
#define PUSHED_AFTER <
#else
#define PUSHED_AFTER >
#endif
  while (local_roots != NULL && 
         (char *) local_roots PUSHED_AFTER caml_exception_pointer) {
    local_roots = local_roots->next;
  }
#undef PUSHED_AFTER

  raise_caml_exception(v);
}

void raise_constant(value tag)
{
  value bucket;
  Begin_root (tag);
    bucket = alloc_small (1, 0);
    Field(bucket, 0) = tag;
  End_roots ();
  mlraise(bucket);
}

void raise_with_arg(value tag, value arg)
{
  value bucket;
  Begin_roots2 (tag, arg);
    bucket = alloc_small (2, 0);
    Field(bucket, 0) = tag;
    Field(bucket, 1) = arg;
  End_roots ();
  mlraise(bucket);
}

void raise_with_string(value tag, char *msg)
{
  raise_with_arg(tag, copy_string(msg));
}

void failwith (char *msg)
{
  raise_with_string((value) Failure, msg);
}

/* We chose to abort the program if a C primitive raises Invalid_argument.
   Rationale: nobody should trap Invalid_argument, and we're not running
   under a toplevel, so this will provide the same feedback to the user.
   Moreover, divisions by zero or out-of-bounds accesses also abort the
   program, and there's no way we can turn them into exceptions.
   Finally, this allows a number of C primitives to be declared "noalloc",
   and this makes calling them much more efficient. */

static struct 
{
  header_t exn_hdr;
  value exn_tag;
  value exn_arg;
} static_bucket;

#define MAXVALLEN 25
static struct
{
  header_t arg_hdr;
  value arg_val;
  value arg_cont[MAXVALLEN];
} static_arg;

void invalid_argument (char *msg)
{
  mlsize_t offset_index;
  mlsize_t len = strlen(msg);
  mlsize_t wosize;

  fprintf(stderr,"Invalid argument: %s\n",msg);
  
  /* setup exception argument */ 
#define result (value) & (static_arg.arg_val)
  if( len > Bsize_wsize(MAXVALLEN)) { len = Bsize_wsize(MAXVALLEN);}
  wosize = (len + sizeof (value)) / sizeof (value);
  static_arg.arg_hdr = Make_header(wosize,String_tag,White);
  Field(result, wosize - 1) = 0;
  strncpy((char*)result, msg, len);
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
#undef result
  
  static_bucket.exn_hdr = Make_header(2,0, White);
  static_bucket.exn_tag = (value) Invalid_argument;
  static_bucket.exn_arg = (value) & (static_arg.arg_val);
  
  mlraise((value) &(static_bucket.exn_tag));
  
  /* fatal_error_arg("Fatal_error: Invalid_argument \"%s\"\n", msg); */
}

/* To raise Out_of_memory, we can't use raise_constant,
   because it allocates and we're out of memory...
   We therefore build the bucket by hand.
   This works OK because the exception value for Out_of_memory is also
   statically allocated out of the heap. */

static struct {
  header_t hdr;
  value exn;
} out_of_memory_bucket;

void raise_out_of_memory(void)
{
  out_of_memory_bucket.hdr = Make_header(1, 0, White);
  out_of_memory_bucket.exn = (value) Out_of_memory;
  mlraise((value) &(out_of_memory_bucket.exn));
}

void raise_sys_error(value msg)
{
  raise_with_arg((value) Sys_error, msg);
}

void raise_end_of_file(void)
{
  raise_constant((value) End_of_file);
}

void raise_zero_divide(void)
{
  raise_constant((value) Division_by_zero);
}

void raise_not_found(void)
{
  raise_constant((value) Not_found);
}
