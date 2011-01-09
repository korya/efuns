/***
To compile:
gcc test.c -o test -I/usr/X11R6/include -I/usr/local/include -L/usr/X11R6/lib -L/usr/local/lib -lX11 -lXext -ljpeg -lpng -ltiff -lz -lgif -lm -lImlib 

Note: found in /usr/doc/imlib-devel-1.9.5
***/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#include <Imlib.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

static Display *disp;
static ImlibData *id;

value image_init(value dname_v)
{
  disp = XOpenDisplay(String_val(dname_v));

 /* Immediately afterwards Intitialise Imlib */
  id=Imlib_init(disp);

  return (disp == NULL) ? Val_true : Val_false;
}

value image_load(value file_v)
{
  ImlibImage *im;
  int w,h;
  value res;

 /* Load the image specified as the first argument */
  im=Imlib_load_image(id,String_val(file_v));

  if (im == NULL) {
   fprintf(stderr, "NULL pointer returned\n");
   failwith("Invalid image loaded");
  }
 /* Suck the image's original width and height out of the Image structure */
  w=im->rgb_width;h=im->rgb_height;

  res = alloc(3,0);
  Field(res,0) = (value)im;
  Field(res,1) = Val_int(w);
  Field(res,2) = Val_int(h);

  return res;
}

value image_pixmap(value image_v, value w_v, value h_v)
{
  Pixmap p,m;
  int w = Int_val(w_v);
  int h = Int_val(h_v);
  ImlibImage *im = (ImlibImage*)Field(image_v,0);
  value res;

 /* Render the original 24-bit Image data into a pixmap of size w * h */
  Imlib_render(id,im,w,h);
 /* Extract the Image and mask pixmaps from the Image */
  p=Imlib_move_image(id,im);
 /* The mask will be 0 if the image has no transparency */
  m=Imlib_move_mask(id,im);

  Begin_root(image_v);
  res = alloc(3,0);
  End_roots();
  
  Field(res,0) = image_v;
  Field(res,1) = Val_int(p);
  Field(res,2) = Val_int(m);

  return res;
}

value pixmap_free(value pixmap_v)
{
  Pixmap p = Int_val(pixmap_v);
  
  Imlib_free_pixmap(id,p);
  return Val_unit;
}


value image_kill(value image_v)
{
  ImlibImage *im = (ImlibImage*)Field(image_v,0);

 /* Render the original 24-bit Image data into a pixmap of size w * h */
  Imlib_kill_image(id,im);
 
  return Val_unit;
}

value image_destroy(value image_v)
{
  ImlibImage *im = (ImlibImage*)Field(image_v,0);

 /* Render the original 24-bit Image data into a pixmap of size w * h */
  Imlib_destroy_image(id,im);
 
  return Val_unit;
}
