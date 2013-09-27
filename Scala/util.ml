
open Str
open String

let camel_of_snake_case =
  let subst s =
    uppercase (sub (matched_string s) 1 1 ) in
  global_substitute (regexp "_\([a-zA-Z]\)") subst

let cc = camel_of_snake_case
let uc s = capitalize (camel_of_snake_case s)
let lc s = uncapitalize (camel_of_snake_case s)

let test () = lc "my_girl_sally"
