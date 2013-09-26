open Camlp4.PreCast
module M = Camlp4OCamlRevisedParser.Make(Syntax)
module N = Camlp4OCamlParser.Make(Syntax)

let files = ref []

let rec do_fn fn =
  let st = Stream.of_channel (open_in fn) in
  let str_item = Syntax.parse_implem (Loc.mk fn) st in
  let str_items = Ast.list_of_str_item str_item [] in
  let tags = List.fold_right do_str_item str_items [] in
  files := (fn, tags)::!files

and do_str_item si tags =
  match si with
 (* | <:str_item< let $rec:_$ $bindings$ >> -> *)
    | Ast.StVal (_, _, bindings) ->
        let bindings = Ast.list_of_binding bindings [] in
        List.fold_right do_binding bindings tags
    | _ -> tags

and do_binding bi tags =
  match bi with
    | <:binding@loc< $lid:lid$ = $_$ >> ->
      let line = Loc.start_line loc in
      let off = Loc.start_off loc in
      let pre = "let " ^ lid in
      (pre, lid, line, off)::tags
    | _ -> tags

let print_tags files =
  let ch = open_out "TAGS" in
  ListLabels.iter files ~f:(fun (fn, tags) ->
    Printf.fprintf ch "\012\n%s,%d\n" fn 0;
    ListLabels.iter tags ~f:(fun (pre, tag, line, off) ->
      Printf.fprintf ch "%s\127%s\001%d,%d\n" pre tag line off))

;;
Arg.parse [] do_fn "otags: fn1 [fn2 ...]";
print_tags !files
