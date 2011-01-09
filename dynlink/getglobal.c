#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"

extern value caml_globals[];
extern char globals_map[];



/*
Problemes avec le GC: certains champs ne sont pas initialises correctement
dans les modules que l'on charge. Il faudrait trouver un moyen d'en tenir
compte.
*/
value asm_getglobal(value i)
{
  return caml_globals[Int_val(i)];
}

value asm_getmap(value unit)
{
  return (value)globals_map;
}

value makeblock(value tag, value size, value accu, value stack,value stp)
{
  int i;
  int sz = Int_val(size);
  int sp = Int_val(stp);
  value res;

  Begin_roots2(accu,stack);
  res = alloc(Int_val(size), Int_val(tag));
  End_roots();
  for(i=1;i<sz;i++) initialize(&Field(res,i),Field(stack,sp++));
  initialize(&Field(res,0), accu);

  return res;
}


value makeblock1(value tag, value accu)
{
  value res;

  Begin_root(accu);
  res = alloc(1, Int_val(tag));
  End_roots();
  initialize(&Field(res,0), accu);

  return res;
}

value makeblock2(value tag, value accu,value arg1)
{
  value res;

  Begin_roots2(accu,arg1);
  res = alloc(2, Int_val(tag));
  End_roots();
  initialize(&Field(res,0), accu);
  initialize(&Field(res,1), arg1);

  return res;
}

value makeblock3(value tag, value accu,value arg1, value arg2)
{
  value res;

  Begin_roots3(accu,arg1,arg2);
  res = alloc(3, Int_val(tag));
  End_roots();
  initialize(&Field(res,0), accu);
  initialize(&Field(res,1), arg1);
  initialize(&Field(res,2), arg2);

  return res;
}
