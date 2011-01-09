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

static value global_table = Val_unit;
static value reify;
static value prims = Val_unit;

#include "caml/callback.h"
#include <stdio.h>

value set_meta(value block)
{
  global_table = Field(block,0);
  reify = Field(block,1);
  prims = Field(block,2);

  register_global_root(&global_table);
  register_global_root(&reify);
  register_global_root(&prims);

  return Val_unit;
}

value get_global_data(value unit)
{
  return global_table;
}

value reify_bytecode(value prog, value len)
{
  return callback2(reify,prog,len);
}

value realloc_global(value size)
{
  raise_not_found ();
}
    
value available_primitives(value unit)
{
  return prims;
}

value invoke_traced_function(value codeptr, value env, value arg)
{
  raise_not_found ();
}
