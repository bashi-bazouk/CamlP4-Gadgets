(*** Module that handles the process top level representation ***)
open Namegen;;
open Piastnode;;
open Equations;;
(***)
exception False;;
exception Found;;
exception Error;;
exception Found_fn;;
(***)
(* Top level process name type *)
type name = | Fname of string | Bname of string | Iname of int;;
(* Top level process action type *)
type top_act =
  { t : Equations.action_type; sub : name; obj : name list; cont : eqvar;
    args : name list
  };;
(* Top level process action set type *)
type top_act_set = top_act array;;
(* Top level process internal action type *)
type top_tau = { tau_cont : eqvar; tau_args : name list };;
(* Top level process internal action set type *)
type top_tau_set = top_tau array;;
(* Top level process test prefix type *)
type top_test =
  { tst : Equations.test_type; idl : name; idr : name; tcont : eqvar;
    targs : name list
  };;
(* Top level process test prefix set type *)
type top_test_set = top_test array;;
(* Top level process sum type *)
type top_sum = ((top_act list) * (top_tau list) * (top_test list));;
(* Top level process sum set type *)
type top_sum_set = top_sum list;;
(* Top level process component type *)
type component =
  { nrests : int; rests : name array; nfnouts : int; nbnouts : int;
    nfninps : int; nbninps : int; ntests : int; ntaus : int; nsums : int;
    fn_outs : top_act_set; bn_outs : top_act_set; fn_inps : top_act_set;
    bn_inps : top_act_set; id_tests : top_test_set; act_taus : top_tau_set;
    act_sums : top_sum_set
  };;
(* Top level process structure type *)
type process_cel =
  { n_comps : int; comps : (component ref) array; env : eq_system;
    fns : eq_fns
  };;
(*** Top level process type ***)
type process = process_cel ref;;
(***)
(* Action kind type *)
type act_kind =
  | ActK of top_act | TestK of top_test | SumK of top_sum | TauK of top_tau;;
(*** Top level process label type ***)
type label = (Equations.action_type * string * (string list));;


(***)
(* Auxiliar functions to print_process *)
let print_name (n: fname) =
  match n with
  | Bname s -> print_string ("bn(" ^ (s ^ ")"))
  | Fname s -> print_string ("fn(" ^ (s ^ ")"))
  | Iname i -> print_string ("in(" ^ ((string_of_int i) ^ ")"));;
let rec print_nameL l =
  match l with
  | [] -> ignore l
  | [ hd ] -> print_name hd
  | hd :: tl -> (print_name hd; print_string ","; print_nameL tl);;
let print_top_act act =
  (print_name act.sub;
   if act.t = Out_type then print_string "!(" else print_string "?(";
   print_nameL act.obj;
   print_string ").";
   print_eqvar act.cont;
   print_string "(";
   print_nameL act.args;
   print_string ")\n");;
let print_top_tau tau =
  (print_string "tau.";
   print_eqvar tau.tau_cont;
   print_string "(";
   print_nameL tau.tau_args;
   print_string ")\n");;
let print_top_test test =
  (print_string "[";
   print_name test.idl;
   if test.tst = Equals_type then print_string "=" else print_string "!=";
   print_name test.idr;
   print_string "].";
   print_eqvar test.tcont;
   print_string "(";
   print_nameL test.targs;
   print_string ")\n");;
let rec print_top_acts_sum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> print_top_act hd
  | hd :: tl -> (print_top_act hd; print_string "+ "; print_top_acts_sum tl);;
let rec print_top_taus_sum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> print_top_tau hd
  | hd :: tl -> (print_top_tau hd; print_string "+ "; print_top_taus_sum tl);;
let rec print_top_tests_sum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> print_top_test hd
  | hd :: tl ->
      (print_top_test hd; print_string "+ "; print_top_tests_sum tl);;
let rec print_top_sums sums =
  match sums with
  | [] -> ignore sums
  | [ (acts, taus, tests) ] ->
      (print_top_acts_sum acts;
       if ((List.length acts) > 0) && ((List.length taus) > 0)
       then print_string "+ "
       else ();
       print_top_taus_sum taus;
       if
         (((List.length acts) + (List.length taus)) > 0) &&
           ((List.length tests) > 0)
       then print_string "+ "
       else ();
       print_top_tests_sum tests)
  | (acts, taus, tests) :: tl ->
      (print_top_acts_sum acts;
       if ((List.length acts) > 0) && ((List.length taus) > 0)
       then print_string "+ "
       else ();
       print_top_taus_sum taus;
       if
         (((List.length acts) + (List.length taus)) > 0) &&
           ((List.length tests) > 0)
       then print_string "+ "
       else ();
       print_top_tests_sum tests;
       print_newline ();
       print_top_sums tl);;
