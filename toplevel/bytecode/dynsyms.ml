
let map () = Symtable.current_state ()
let install_modules _ = ()
let alloc_code code code_size = code
let free_code code = Meta.static_free code
  