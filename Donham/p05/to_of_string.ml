module Make (AstFilters : Camlp4.Sig.AstFilters) = 
struct 
  open AstFilters

  let rec filter si = 
    match wrap_str_item si with 
      | <:str_item< type $lid:tid$ = $Ast.TySum (_, ors)$ >> -> 
	  begin
	    print_endline "match found";
            try 
              let cons = 
                List.map 
                  (function 
                     | <:ctyp< $uid: c$ >> -> c 
                     | _ -> raise Exit) 
                  (Ast.list_of_ctyp ors []) in 
              to_of_string si tid cons 
            with Exit -> si 
          end 
       | _ -> 
	 print_endline "didn't match";
	 si

  and wrap_str_item si = 
    let _loc = Ast.loc_of_str_item si in 
    <:str_item< $si$ >>

  and to_of_string si tid cons = 
    let _loc = Ast.loc_of_str_item si in 
    <:str_item< 
      $si$;; 
      $to_string _loc tid cons$;; 
      $of_string _loc tid cons$;; 
    >>

  and to_string _loc tid cons = 
    <:str_item< 
      let $lid: tid ^ "_to_string"$ = function 
        $list: 
          List.map 
            (fun c -> <:match_case< $uid: c$ -> $`str: c$ >>) 
            cons$ 
    >>

  and of_string _loc tid cons = 
    <:str_item< 
      let $lid: tid ^ "_of_string"$ = function 
        $list: 
          List.map 
            (fun c -> <:match_case< 
       $`str: c$ -> $uid: c$
     >>) 
            cons$ 
        | _ -> invalid_arg "bad string" 
    >>

  ;; 
  AstFilters.register_str_item_filter begin fun si -> 
    let _loc = Ast.loc_of_str_item si in 
    <:str_item< 
      $list: List.map filter (Ast.list_of_str_item si [])$ 
    >> 
  end

end 
module Id = 
struct 
  let name = "to_of_string" 
  let version = "0.1" 
end 
;; 
let module M = Camlp4.Register.AstFilter(Id)(Make) in () 
