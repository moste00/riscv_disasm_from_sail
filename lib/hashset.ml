type 'a set = ('a, unit) Hashtbl.t

let set_add set thing = Hashtbl.add set thing ()

let set_contains set thing =
  try
    Hashtbl.find set thing;
    true
  with Not_found -> false
