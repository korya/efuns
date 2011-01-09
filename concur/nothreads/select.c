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

/* $Id: select.c,v 1.3 1999/12/30 08:07:19 lefessan Exp $ */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

void enter_blocking_section (void);
void leave_blocking_section (void);

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>

#endif

#ifdef __OpenBSD__
#include <string.h>
#endif

#ifdef FD_ISSET
typedef fd_set file_descr_set;
#else
typedef int file_descr_set;
#define FD_SETSIZE (sizeof(int) * 8)
#define FD_SET(fd,fds) (*(fds) |= 1 << (fd))
#define FD_CLR(fd,fds) (*(fds) &= ~(1 << (fd)))
#define FD_ISSET(fd,fds) (*(fds) & (1 << (fd)))
#define FD_ZERO(fds) (*(fds) = 0)
#endif

static void fdlist_to_fdset(value fdlist, file_descr_set *fdset)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    FD_SET(Int_val(Field(l, 0)), fdset);
  }
}

static value fdset_to_fdlist(file_descr_set *fdset)
{
  int i;
  value res = Val_int(0);

  Begin_root(res);
    for (i = FD_SETSIZE - 1; i >= 0; i--) {
      if (FD_ISSET(i, fdset)) {
	value newres = alloc_small(2, 0);
	Field(newres, 0) = Val_int(i);
	Field(newres, 1) = res;
	res = newres;
      }
    }
  End_roots();
  return res;
}

value my_select(value readfds) /* ML */
{
  file_descr_set read;
  int retcode;
  value read_list = Val_unit;

  Begin_root (read_list);
    fdlist_to_fdset(readfds, &read);
    enter_blocking_section();
    retcode = select(FD_SETSIZE, &read, NULL, NULL, NULL);
    leave_blocking_section();
    if (retcode == -1) failwith ("Error in select");
    read_list = fdset_to_fdlist(&read);
  End_roots();
  return read_list;
}

value my_select1(value fds) /* ML */
{
  file_descr_set read;
  int retcode;
  int fdset = Int_val(fds);

  FD_ZERO(&read); 
  FD_SET(fdset, &read);

    enter_blocking_section();
    retcode = select(FD_SETSIZE, &read, NULL, NULL, NULL);
    leave_blocking_section();

    if (retcode == -1) failwith ("Error in select");
  if(FD_ISSET(fdset, &read)) return Val_true;
  return Val_false;

}

value my_select_no_wait(value readfds) /* ML */
{
  file_descr_set read;
  int retcode;
  value read_list = Val_unit;
  struct timeval delay_tv, * delay_ptr;

  delay_tv.tv_sec = 0;
  delay_tv.tv_usec = 0;
  delay_ptr = &delay_tv;

  Begin_root (read_list);
    fdlist_to_fdset(readfds, &read);
    enter_blocking_section();
    retcode = select(FD_SETSIZE, &read, NULL, NULL, delay_ptr);
    leave_blocking_section();
    if (retcode == -1) failwith ("Error in select");
    read_list = fdset_to_fdlist(&read);
  End_roots();
  return read_list;
}

