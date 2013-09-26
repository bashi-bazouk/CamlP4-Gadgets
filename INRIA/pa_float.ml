module Id = struct
  let name = "pa_float"
  let version = "1.0"
end

open Camlp4

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  class ['a] float_subst _loc = object
    inherit Ast.map as super
    method _Loc_t (_ : 'a) = _loc
    method expr =
      function
	| <:expr< ( + ) >> -> <:expr< ( +. ) >>
	| <:expr< ( - ) >> -> <:expr< ( -. ) >>
	| <:expr< ( * ) >> -> <:expr< ( *. ) >>
	| <:expr< ( / ) >> -> <:expr< ( /. ) >>
	| <:expr< $int:i$ >> ->
          let f = float(int_of_string i) in <:expr< $`flo:f$ >>
	| e -> super#expr e
  end;;

  EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "simple"
    [ [ "Float"; "."; "("; e = SELF; ")" -> (new float_subst _loc)#expr e ]
    ]
    ;
    END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
