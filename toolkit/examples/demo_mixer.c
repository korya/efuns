/***********************************************************************/
/*                                                                     */
/*                             ____                                    */
/*                                                                     */
/*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef __linux__
#include <stdio.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#else
#define SOUND_MIXER_NRDEVICES 1
#define SOUND_DEVICE_NAMES { "No device" }
#define SOUND_DEVICE_LABELS { "No device" }
#endif

value mixer_init(value mixer)
{

#ifdef __linux__
  int fd = Int_val(mixer);
  int dev_mask, rec_mask, stereo_mask;
  int i,n=0;
  int devices[SOUND_MIXER_NRDEVICES];
  value res;
  
  if (ioctl (fd, SOUND_MIXER_READ_DEVMASK, &dev_mask) == -1) {
    failwith("Mixer: cannot read devmask");    
  }
  rec_mask = 0;  
  if (ioctl (fd, SOUND_MIXER_READ_RECMASK, &rec_mask) == -1) {
    failwith("Mixer: cannot read recmask");    
  }
  stereo_mask = 0;  
  if (ioctl (fd, SOUND_MIXER_READ_STEREODEVS, &stereo_mask) == -1) {
    failwith("Mixer: cannot read stereo");    
  }

  n=0;
  for (i = 0; i < SOUND_MIXER_NRDEVICES; i++)  {
    if ((1 << i) & dev_mask) {
      devices[n++] = i;
    }
  }
 
  res = alloc(3*n,0);
  for(i=0;i<n;i++){
    Field(res,3*i) = Val_int(devices[i]);
    Field(res,3*i+1) = Val_int(!!((1 << devices[i]) & stereo_mask));
    Field(res,3*i+2) = Val_int(!!(((1 << devices[i]) & rec_mask)));
  }
  return res;
#else
  return Atom(0);
#endif
}

value mixer_label(value dev)
{
  char *labels[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_LABELS;
  char *names[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_NAMES;
  value s1,s2,res;
  value i = Int_val(dev);

  s1 = copy_string(labels[i]);
  Begin_root(s1);
  s2 = copy_string(names[i]);
  Begin_root(s2);
  res = alloc(2,0);
  initialize(&Field(res,0),s1);
  initialize(&Field(res,1),s2);
  End_roots();
  End_roots();
  
  return res;
}

value get_volume (value mixer, value dev) {
  int fd = Int_val(mixer);
  int i = Int_val(dev);
  int level=0;

#ifdef __linux__  
  ioctl (fd, MIXER_READ(i), &level);
#endif

  return Val_int(level);
}

value set_volume(value mixer, value dev, value init){
  int fd = Int_val(mixer);
  int i = Int_val(dev);
  int level = Int_val(init);

#ifdef __linux__
  ioctl (fd, MIXER_WRITE(i), &level);
#endif

  return Val_int(level);
}

value is_recording(value mixer, value dev){
  int fd = Int_val(mixer);
  int i = Int_val(dev);
  int source=0;

#ifdef __linux__  
  ioctl (fd, SOUND_MIXER_READ_RECSRC, &source);
#endif

  return Val_int( (((1 << i) & source)>>i));
}

value set_recording (value mixer, value dev, value bool) {
  int fd = Int_val(mixer);
  int i = Int_val(dev);
  int source;

#ifdef __linux__  
  ioctl (fd, SOUND_MIXER_READ_RECSRC, &source);
  if (Int_val(bool)) { 
    source = source | (1 << i); 
  } else { 
    source = source & ~(1 << i); 
  }
  ioctl (fd, SOUND_MIXER_WRITE_RECSRC, &source);
#endif

  return Val_unit;
}

