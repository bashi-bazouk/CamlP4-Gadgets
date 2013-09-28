open Format;
open Camlp4;
open Util;

module Id: Sig.Id = struct
  value name = "Camlp4.Printers.ScalaOfOCaml";
  value version = Sys.ocaml_version;
end;

module ScalaOfOCaml (Syntax : Sig.Camlp4Syntax): (Sig.Printer Syntax.Ast).S = struct
  include Syntax;

  module OCaml = OCaml.Make(Syntax);
  open OCaml;

  module Lexer = Struct.Lexer.Make Token;
  let module M = ErrorHandler.Register Lexer.Error in ();
  open Sig;

  class printer ?curry_constr:(init_curry_constr = False) ?(comments = True) () = object (o)
    inherit OCaml.printer ~curry_constr:init_curry_constr ~comments:comments () as super;

    value semisep : sep = "";

    method andsep : sep = "@]@ @[<2>def@ ";
    method value_let = "def"; (* TODO: distinguish vals and defs *)
      
    method var f = fun
      [ "[]" -> pp f "Nil"
      | "()" -> pp f "()"
      | " True"  -> pp f "True"
      | " False" -> pp f "False"
      | tok -> 
        match lex_string tok with
	    [ LIDENT s | UIDENT s | ESCAPED_IDENT s ->
              pp_print_string f (cc s)
	    | _ -> super#var f tok ]];

    (* New: Used when a type-variable is distinguished from a regular one. *)
    method tvar f = 
      fun
      [ ""   -> pp f "$lid:\"\"$"
      | "()" -> pp f "Unit"
      | v ->
          match lex_string v with
            [ (LIDENT s | UIDENT s | ESCAPED_IDENT s) when is_keyword s ->
                pp f "%s__" s
            | (LIDENT s | ESCAPED_IDENT s) when List.mem s infix_lidents ->
                pp f "( %s )" s
            | SYMBOL s ->
                pp f "( %s )" s
            | LIDENT s | UIDENT s | ESCAPED_IDENT s ->
                pp_print_string f (uc s)
            | tok -> failwith (sprintf
                      "Bad token used as an identifier: %s"
                      (Token.to_string tok))]];

    method match_case f =
      fun
      [ <:match_case@_loc<>> ->
        pp f "@[<2>@ case _ =>@ %a@]" o#raise_match_failure _loc
      | tok -> super#match_case f tok];
  
    method match_case_aux f =
      fun
      [ <:match_case< $p$ -> $e$ >> ->
          pp f "@ case @[<2>%a@ =>@ %a@]" o#patt p o#under_pipe#expr e
      | <:match_case< $p$ when $w$ -> $e$ >> ->
          pp f "@ case @[<2>%a@ when@ %a@ =>@ %a@]"
            o#patt p o#under_pipe#expr w o#under_pipe#expr e
      | tok -> super#match_case_aux f tok];

    method binding f bi =
      let () = o#node f bi Ast.loc_of_binding in
      match bi with
      [ <:binding<>> -> ()
      | <:binding< $b1$ and $b2$ >> ->
          do { o#binding f b1; pp f o#andsep; o#binding f b2 }
      | <:binding< $p$ = $e$ >> ->
          let (pl, e) =
            match p with
            [ <:patt< ($_$ : $_$) >> -> ([], e)
            | _ -> expr_fun_args e ] in
          match (p, e) with
          [ (<:patt< $lid:_$ >>, <:expr< ($e$ : $t$) >>) ->
              pp f "%a :@ %a =@ {@\n %a }"
                (list o#fun_binding "@ ") [`patt p::pl] o#ctyp t o#expr e
          | _ -> pp f "%a@[<0>(%a) = {@]@ %a }@\n" o#simple_patt
                    p (list' o#fun_binding ", " "") pl o#expr e ]
      | <:binding< $anti:s$ >> -> o#anti f s ];

    method sum_type f t =
      match Ast.list_of_ctyp t [] with
      [ [] -> ()
      | ts ->
          pp f "@[<hv0>case class %a@]" (list o#ctyp "@\ncase class ") ts ];

    method type_ident f i =
    let () = o#node f i Ast.loc_of_ident in
    match i with
    [ <:ident< $i1$.$i2$ >> -> pp f "%a.@,%a" o#type_ident i1 o#type_ident i2
    | <:ident< $i1$ $i2$ >> -> pp f "%a@,(%a)" o#type_ident i1 o#type_ident i2
    | <:ident< $anti:s$ >> -> o#anti f s
    | <:ident< $lid:s$ >> | <:ident< $uid:s$ >> -> o#tvar f s ];

    method expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
	[ <:expr< $x$ $y$ >> ->
          let (a, al) = get_expr_args x [y] in
          if (not curry_constr) && Ast.is_expr_constructor a then
            match al with
		[ [ <:expr< ($tup:_$) >> ] ->
		  pp f "@[<2>%a(%a)@]" o#apply_expr x o#expr y
		| [_] -> pp f "@[<2>%a(%a)@]" o#apply_expr x o#apply_expr y
		| al ->
		  pp f "@[<2>%a(%a)@]" o#apply_expr a
                 (* The #apply_expr below may put too much parens.
                    However using #expr would be wrong: PR#5056. *)
                    (list o#under_pipe#apply_expr ",@ ") al ]
          else pp f "@[<2>%a(%a)@]" o#apply_expr a (list o#apply_expr ",@ ") al
	| <:expr< fun [ $a$ ] >> ->
	  pp f "@[<hv0>_ match { %a }@]" o#match_case a
	| <:expr< if $e1$ then $e2$ else $e3$ >> ->
          pp f "@[<hv0>@[<2>if(%a)@]@ {@[<2>@ %a@] } else @[<2>{@ %a }@]@]"
           o#expr e1 o#under_semi#expr e2 o#under_semi#expr e3
	| <:expr< let $rec:r$ $bi$ in $e$ >> ->
          match e with
              [ <:expr< let $rec:_$ $_$ in $_$ >> ->
		pp f "@[<0>@[<2>def %a@]@ %a@]"
		  o#binding bi o#reset_semi#expr e
              | _ ->
		pp f "@[<hv0>@[<2>def %a@]@ @[<hv2>%a@]@]"
		  o#binding bi o#reset_semi#expr e ]
	| <:expr< match $e$ with [ $a$ ] >> ->
          pp f "@[<hv0>@[<hv0>@[<2>%a match@]@ { @]%a }@]"
	    o#expr e o#match_case a
	| e ->super#expr f e ];

    method dot_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< $e1$ .( $e2$ ) >> ->
        pp f "@[<2>%a@,(%a)@]" o#dot_expr e1 o#expr e2
    | <:expr< $e1$ .[ $e2$ ] >> ->
        pp f "%a@[<1>(@,%a@)@,]" o#dot_expr e1 o#expr e2
    | e -> super#dot_expr f e ];

    method simple_expr f e =
    let () = o#node f e Ast.loc_of_expr in
    match e with
    [ <:expr< for $s$ = $e1$ $to:df$ $e2$ do { $e3$ } >> ->
      pp f "@[<hv0>@[<hv2>@[<2>for(%a <-@ Range(%a,@ %a))@ {@]@ %a@]@ }@]"
        o#var s o#expr e1 o#expr e2 o#seq e3
    | e -> super#simple_expr f e];

    method patt4 f = fun
    [ <:patt< [$_$ :: $_$] >> as p ->
        let (pl, c) = o#mk_patt_list p in
        match c with
        [ None -> pp f "@[<2>List(%a) @]" (list o#patt ",@ ") pl
        | Some x -> pp f "@[<2>%a::List(%a)@]" (list o#patt5 " ::@ ") pl o#patt5 x]
    | p -> o#patt5 f p ];

    method simple_patt f p =
    let () = o#node f p Ast.loc_of_patt in
    match p with
    [ <:patt< ( $p$ : $t$ ) >> -> pp f "@[<1>%a :@ %a@]" o#patt p o#ctyp t
    | _ -> super#simple_patt f p];

    method simple_ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ <:ctyp< $id:i$ >> -> o#type_ident f i
    | <:ctyp< '$s$ >> -> pp f "'%a" o#tvar s
    | <:ctyp< # $i$ >> -> pp f "@[<2>#%a@]" o#type_ident i
    | <:ctyp< $t1$ * $t2$ >> -> pp f "%a,@ %a" o#simple_ctyp t1 o#simple_ctyp t2
    | _ -> super#simple_ctyp f t];

    method ctyp f t =
    let () = o#node f t Ast.loc_of_ctyp in
    match t with
    [ <:ctyp< +'$s$ >> -> pp f "+'%a" o#tvar s
    | <:ctyp< -'$s$ >> -> pp f "-'%a" o#tvar s
    | <:ctyp< $t1$ of $t2$ >> ->
      pp f "@[<h>%a(@[<3>FIXME: %a)@]@]" o#ctyp t1 o#constructor_type t2
    | Ast.TyDcl _ tn tp te cl -> do {
        pp f "@[<2>%a%a@]" o#type_params tp o#tvar tn;
        match te with
        [ <:ctyp<>> -> ()
        | _ -> pp f " =@ %a" o#ctyp te ]; (* HERE *)
        if cl <> [] then pp f "@ %a" (list o#constrain "@ ") cl else ();
      }
    | _ -> super#ctyp f t];

    method ctyp1 f = fun
    [ <:ctyp< $t1$ $t2$ >> ->
        match get_ctyp_args t1 [t2] with
        [ (_, [_]) -> pp f "@[<2>%a[%a]@]" o#simple_ctyp t1 o#simple_ctyp t2
        | (a, al) -> pp f "@[<2>%a[(%a)]@]" o#simple_ctyp a (list o#ctyp ",@ ") al ]
    | <:ctyp< ! $t1$ . $t2$ >> ->
        let (a, al) = get_ctyp_args t1 [] in
        pp f "@[<2>%a.@ %a@]" (list o#ctyp "@ ") [a::al] o#ctyp t2
    | <:ctyp< private $t$ >> -> pp f "@[private@ %a@]" o#simple_ctyp t
    | t -> o#simple_ctyp f t ];

    method str_item f st =
      let () = o#node f st Ast.loc_of_str_item in
      match st with
      [ <:str_item< exception $t$ >> ->
        pp f "@[<2>class@ %a%(%) extends Exception@]" o#ctyp t semisep
      | <:str_item< module $s1$ ($s2$ : $mt1$) = $me$ >> ->
          match o#module_expr_get_functor_args [(s2, mt1)] me with
          [ (al, me, Some mt2) ->
            pp f "@[<2>module %a@ @[<0>%a@] :@ %a =@ %a%(%)@]"
              o#tvar s1 o#functor_args al o#module_type mt2
              o#module_expr me semisep
          | (al, me, _) ->
            pp f "@[<2>module %a@ @[<0>%a@] =@ %a%(%)@]"
              o#tvar s1 o#functor_args al o#module_expr me semisep ]
      | <:str_item< module $s$ : $mt$ = $me$ >> ->
        pp f "@[<2>module %a :@ %a =@ %a%(%)@]"
          o#tvar s o#module_type mt o#module_expr me semisep
      | <:str_item< module $s$ = $me$ >> ->
        pp f "@[<2>module %a =@ %a%(%)@]" o#tvar s o#module_expr me semisep      
      | <:str_item< module type $s$ = $mt$ >> ->
	pp f "@[<2>module type %a =@ %a%(%)@]"
	  o#tvar s o#module_type mt semisep
      | <:str_item< open $sl$ >> ->
        pp f "@[<2>import@ %a%(%)@]" o#type_ident sl semisep
      | <:str_item< type $Ast.TyDcl(_, tn, tp, Ast.TySum(_, ts), cl)$>> -> (* HERE *)
	o#scala_sum f tn tp ts cl
      | <:str_item< type $Ast.TyDcl(_, tn, tp, Ast.TyRec(_, cs), cl)$>> -> (* HERE *)
	o#scala_record f tn tp cs cl
      | <:str_item< type $t$ >> ->
	pp f "@\n@[<hv0>@[<hv2>type %a@]%(%)@]@\n" o#ctyp t semisep      
      | <:str_item< $exp:e$ >> ->
        pp f "@[<2>def _ =@ %a%(%)@]" o#expr e semisep
      | _ -> super#str_item f st];

    method scala_record f tn tp cs cl =
      let tn = uc tn in
      let rec member fst f = fun
	[ Ast.TySem (_,a,b) ->
	    pp f "%a,@\n%a" (member fst) a (member False) b
	| Ast.TyCol(_,id,typ) ->
	  if fst then
	    pp f "@[%a: %a@]" o#record_name id o#ctyp typ
	  else
	    pp f "@[<2>%a: %a@]" o#record_name id o#ctyp typ
	| _ -> assert False] in
      pp f "@[class %s(@[%a)@]" tn (member True) cs;

    method record_name f = fun
    [ <:ctyp< $id:i$ >> -> o#ident f i
    | t -> assert False ];

    method scala_sum f tn tp ts cl =
      let tn = uc tn in
      let rec case f = fun
	[ Ast.TyOr(_, l, r) ->
	  pp f "%a@\n%a" case l case r
	| t ->
	  pp f "@[case class %a extends %s@]" o#ctyp t tn] in
      pp f "@[abstract class %s()@\n%a@] " tn case ts;

    method module_type f mt =
    let () = o#node f mt Ast.loc_of_module_type in
    match mt with
    [ <:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >> ->
          pp f "@[<2>functor@ @[<1>(%a :@ %a)@]@ ->@ %a@]"
            o#tvar s o#module_type mt1 o#module_type mt2
    | <:module_type< '$s$ >> -> pp f "'%a" o#tvar s
    | _ -> super#module_type f mt];

    method simple_module_expr f me =
    let () = o#node f me Ast.loc_of_module_expr in
    match me with
    [ <:module_expr< functor ( $s$ : $mt$ ) -> $me$ >> ->
      pp f "@[<2>functor@ @[<1>(%a :@ %a)@]@ ->@ %a@]" 
	o#tvar s o#module_type mt o#module_expr me
    | _ -> super#simple_module_expr f me];

    method class_expr f ce =
    let () = o#node f ce Ast.loc_of_class_expr in
    match ce with
    [ <:class_expr< virtual $lid:i$ >> ->
      pp f "@[<2>virtual@ %a@]" o#tvar i
    | _ -> super#class_expr f ce];
	    
  end;

  value with_outfile output_file fct arg =
    let call close f = do {
      try fct f arg with [ exn -> do { close (); raise exn } ];
      close ()
    } in
    match output_file with
    [ None -> call (fun () -> ()) std_formatter
    | Some s ->
        let oc = open_out s in
        let f = formatter_of_out_channel oc in
        call (fun () -> close_out oc) f ];

  value print output_file fct =
    let o = new printer () in
    with_outfile output_file (fct o);

  value print_interf ?input_file:(_) ?output_file sg =
    print output_file (fun o -> o#interf) sg;

  value print_implem ?input_file:(_) ?output_file st =
    print output_file (fun o -> o#implem) st;
end;

module M = Camlp4.Register.OCamlPrinter(Id)(ScalaOfOCaml);
