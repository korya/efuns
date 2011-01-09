/***********************************************************************/
/*                                                                     */
/*                             GwML                                    */
/*                                                                     */
/*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <signal.h>
#include <esd.h>
#include <stdio.h>

static int esd;

#define ESD_CONNECTED_P	(esd > 0)

#define SOUND(v) (Int_val(v)-1)
#define VALSOUND(v) (Val_int(v+1))

value sound_load(value file_v, value name_v){
  char *file = String_val(file_v);
  char *name = String_val(name_v);
  value r = Val_int(-1);
    
  if (ESD_CONNECTED_P) {
    char *path, *tag;
    int sample;

    path = file;
    if (strlen(name)==0) tag = "gwml";
    else
      tag = name;
    sample = esd_file_cache(esd, tag, path);
    r = VALSOUND(sample);
  } else {
    r = Val_int(0);
  }
  return r;
}

value sound_unload(value sound_v){
  int sound = SOUND(sound_v);

  if (ESD_CONNECTED_P) {
    esd_sample_free(esd, sound);
  }
  return Val_unit;
}

value sound_play(value sound_v)
{
  int sound = SOUND(sound_v);

  if (ESD_CONNECTED_P) {
    int sample;

    sample = sound;
    esd_sample_play(esd, sample);
  } 
  return Val_unit;
}

value file_play(value sound_v)
{
  char *sound = String_val(sound_v);

  char *path = sound;

  esd_play_file("gwml", path, 1);
  return Val_unit;
}

value esd_reconnect(value host_v)
{
  char *hostname = String_val(host_v);

  if (strlen(hostname) == 0) hostname = NULL;

  if (ESD_CONNECTED_P)
    esd_close(esd);
  esd = esd_open_sound(hostname);

  return Val_unit;
}

void catch_pipe()
{
  fprintf(stderr, "-SIGPIPE-");
}

value init_sound(value host_v) /* ML */
{
  char *hostname = String_val(host_v);

  if (strlen(hostname) == 0) hostname = NULL;

  esd = esd_open_sound(hostname);
  signal(SIGPIPE, catch_pipe);
  return Val_unit;
}
