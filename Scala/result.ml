(*** Module that handles the process top level representation ***)
import Namegen
import Piastnode
import Equations
(***)
class False() extends Exception
class Found() extends Exception
class Error() extends Exception
class FoundFn() extends Exception
(***)
(* Top level process name type *)
abstract class Name()
case class Fname(FIXME: String) extends Name
case class Bname(FIXME: String) extends Name
case class Iname(FIXME: Int) extends Name 
(* Top level process action type *)
class TopAct(t: Equations.ActionType,
             sub: Name,
             obj: List[Name],
             cont: Eqvar,
             args: List[Name])
(* Top level process action set type *) 
type TopActSet = Array[TopAct]
 (* Top level process internal action type *)
class TopTau(tauCont: Eqvar,
             tauArgs: List[Name])
(* Top level process internal action set type *)

type TopTauSet = Array[TopTau]
 (* Top level process test prefix type *)
class TopTest(tst: Equations.TestType,
              idl: Name,
              idr: Name,
              tcont: Eqvar,
              targs: List[Name])
(* Top level process test prefix set type *) 
type TopTestSet = Array[TopTest]
 (* Top level process sum type *)

type TopSum = ((List[TopAct]), (List[TopTau]), (List[TopTest]))
 (* Top level process sum set type *) 
type TopSumSet = List[TopSum]
 (* Top level process component type *)
class Component(nrests: Int,
                rests: Array[Name],
                nfnouts: Int,
                nbnouts: Int,
                nfninps: Int,
                nbninps: Int,
                ntests: Int,
                ntaus: Int,
                nsums: Int,
                fnOuts: TopActSet,
                bnOuts: TopActSet,
                fnInps: TopActSet,
                bnInps: TopActSet,
                idTests: TopTestSet,
                actTaus: TopTauSet,
                actSums: TopSumSet)
(* Top level process structure type *)
class ProcessCel(nComps: Int,
                 comps: Array[(Ref[Component])],
                 env: EqSystem,
                 fns: EqFns)
(*** Top level process type ***) 
type Process = Ref[ProcessCel]
 (***) (* Action kind type *)
abstract class ActKind()
case class ActK(FIXME: TopAct) extends ActKind
case class TestK(FIXME: TopTest) extends ActKind
case class SumK(FIXME: TopSum) extends ActKind
case class TauK(FIXME: TopTau) extends ActKind 
(*** Top level process label type ***)

type Label = (Equations.ActionType, String, (List[String]))
 (***) (* Auxiliar functions to print_process *)
let printName n =
  match n with
  | bname s -> printString ("bn(" ^ (s ^ ")"))
  | fname s -> printString ("fn(" ^ (s ^ ")"))
  | iname i -> printString ("in(" ^ ((stringOfInt i) ^ ")"))
let rec printNameL l =
  match l with
  | [] -> ignore l
  | [ hd ] -> printName hd
  | hd :: tl -> (printName hd; printString ","; printNameL tl)
let printTopAct act =
  (printName act.sub;
   if act.t = outType then printString "!(" else printString "?(";
   printNameL act.obj;
   printString ").";
   printEqvar act.cont;
   printString "(";
   printNameL act.args;
   printString ")\n")
let printTopTau tau =
  (printString "tau.";
   printEqvar tau.tauCont;
   printString "(";
   printNameL tau.tauArgs;
   printString ")\n")
let printTopTest test =
  (printString "[";
   printName test.idl;
   if test.tst = equalsType then printString "=" else printString "!=";
   printName test.idr;
   printString "].";
   printEqvar test.tcont;
   printString "(";
   printNameL test.targs;
   printString ")\n")
let rec printTopActsSum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> printTopAct hd
  | hd :: tl -> (printTopAct hd; printString "+ "; printTopActsSum tl)
let rec printTopTausSum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> printTopTau hd
  | hd :: tl -> (printTopTau hd; printString "+ "; printTopTausSum tl)
let rec printTopTestsSum s =
  match s with
  | [] -> ignore s
  | [ hd ] -> printTopTest hd
  | hd :: tl -> (printTopTest hd; printString "+ "; printTopTestsSum tl)
let rec printTopSums sums =
  match sums with
  | [] -> ignore sums
  | [ (acts, taus, tests) ] ->
      (printTopActsSum acts;
       if ((list.length acts) > 0) && ((list.length taus) > 0)
       then printString "+ "
       else ();
       printTopTausSum taus;
       if
         (((list.length acts) + (list.length taus)) > 0) &&
           ((list.length tests) > 0)
       then printString "+ "
       else ();
       printTopTestsSum tests)
  | (acts, taus, tests) :: tl ->
      (printTopActsSum acts;
       if ((list.length acts) > 0) && ((list.length taus) > 0)
       then printString "+ "
       else ();
       printTopTausSum taus;
       if
         (((list.length acts) + (list.length taus)) > 0) &&
           ((list.length tests) > 0)
       then printString "+ "
       else ();
       printTopTestsSum tests;
       printNewline ();
       printTopSums tl)
(***)
let printComponent comp =
  (printString "- COMP -\nrestricted: ";
   for i = 0 to !comp.nrests - 1 do
     printName !comp.rests.(i);
     printString " "
   done;
   printString "\nfnouts: ";
   printInt !comp.nfnouts;
   printNewline ();
   for i = 0 to !comp.nfnouts - 1 do printTopAct !comp.fnOuts.(i) done;
   printString "fninps: ";
   printInt !comp.nfninps;
   printNewline ();
   for i = 0 to !comp.nfninps - 1 do printTopAct !comp.fnInps.(i) done;
   printString "bnouts: ";
   printInt !comp.nbnouts;
   printNewline ();
   for i = 0 to !comp.nbnouts - 1 do printTopAct !comp.bnOuts.(i) done;
   printString "bninps: ";
   printInt !comp.nbninps;
   printNewline ();
   for i = 0 to !comp.nbninps - 1 do printTopAct !comp.bnInps.(i) done;
   printString "taus: ";
   printInt !comp.ntaus;
   printNewline ();
   for i = 0 to !comp.ntaus - 1 do printTopTau !comp.actTaus.(i) done;
   printString "tests: ";
   printInt !comp.ntests;
   printNewline ();
   for i = 0 to !comp.ntests - 1 do printTopTest !comp.idTests.(i) done;
   printString "sums: ";
   printInt !comp.nsums;
   printNewline ();
   printTopSums !comp.actSums)
(*** Prints a top level process ***)
let printProcess p =
  (printString "*** PROCESS ***\n";
   for i = 0 to (array.length !p.comps) - 1 do printComponent !p.comps.(i)
   done;
   printString "***************\n")
(***) (* Auxiliar functions *)
let freshRests size =
  let res = array.create size ""
  in (for i = 0 to size - 1 do res.(i) <- genBname () done; res)
(***) let getString n = match n with | bname s -> s | fname s -> s | _ -> ""
let rec getStringL nL =
  match nL with | [] -> [] | hd :: tl -> (getString hd) :: (getStringL tl)
(***)
let nilComponent () =
  {
    nrests = 0;
    rests = [|  |];
    nfnouts = 0;
    nbnouts = 0;
    nfninps = 0;
    nbninps = 0;
    ntests = 0;
    ntaus = 0;
    nsums = 0;
    fnOuts = [|  |];
    bnOuts = [|  |];
    fnInps = [|  |];
    bnInps = [|  |];
    idTests = [|  |];
    actTaus = [|  |];
    actSums = [];
  }
(***)
let newComponent rL fnoL bnoL fniL bniL tests taus sums =
  {
    nrests = list.length rL;
    rests = array.ofList rL;
    nfnouts = list.length fnoL;
    nbnouts = list.length bnoL;
    nfninps = list.length fniL;
    nbninps = list.length bniL;
    ntests = list.length tests;
    ntaus = list.length taus;
    nsums = list.length sums;
    fnOuts = array.ofList fnoL;
    bnOuts = array.ofList bnoL;
    fnInps = array.ofList fniL;
    bnInps = array.ofList bniL;
    idTests = array.ofList tests;
    actTaus = array.ofList taus;
    actSums = sums;
  }
(***) (* Auxiliar functions to handle component identification *)
let rec getComps a acts numActs names numNames part j =
  for k = 0 to numActs - 1 do
    if (acts.(k) = 0) && a.(k).(j)
    then (acts.(k) <- part; getNames a acts numActs names numNames part k)
    else ()
  done
and getNames a acts numActs names numNames part i =
  for j = 0 to numNames - 1 do
    if (names.(j) = 0) && a.(i).(j)
    then (names.(j) <- part; getComps a acts numActs names numNames part j)
    else ()
  done
let comps a =
  let numActs = array.length a in
  let numNames = array.length a.(0) in
  let parts = ref 1 in
  let acts = array.create numActs 0 in
  let names = array.create numNames 0
  in
    (for i = 0 to numActs - 1 do
       if acts.(i) = 0
       then
         (acts.(i) <- !parts;
          getNames a acts numActs names numNames !parts i;
          incr parts)
       else ()
     done;
     ((!parts - 1), acts, names))
(***) (* Auxiliar functions to handle top level process action creation *)
let eqName n marker pos subArgs subRests parsMarker =
  match n with
  | rn i -> (marker.(pos).(i) <- true; bname subRests.(i))
  | pn i ->
      ((try
          let k = hashtbl.find !parsMarker (getString subArgs.(i))
          in marker.(pos).(k) <- true
        with | notFound -> ignore i);
       subArgs.(i))
  | fn s -> fname s
  | in i -> iname i
let rec eqNameL l marker pos subArgs subRests parsMarker =
  match l with
  | [] -> []
  | hd :: tl ->
      (eqName hd marker pos subArgs subRests parsMarker) ::
        (eqNameL tl marker pos subArgs subRests parsMarker)
let eqAct act marker subArgs subRests pos parsMarker =
  match act with
  | (eqT, s, o, x, a) ->
      {
        t = eqT;
        sub = eqName s marker pos subArgs subRests parsMarker;
        obj = eqNameL o marker pos subArgs subRests parsMarker;
        cont = x;
        args = eqNameL a marker pos subArgs subRests parsMarker;
      }
let eqTest test marker subArgs subRests pos parsMarker =
  match test with
  | (typ, id1, id2, x, a) ->
      {
        tst = typ;
        idl = eqName id1 marker pos subArgs subRests parsMarker;
        idr = eqName id2 marker pos subArgs subRests parsMarker;
        tcont = x;
        targs = eqNameL a marker pos subArgs subRests parsMarker;
      }
let eqTau tau marker subArgs subRests pos parsMarker =
  match tau with
  | (x, a) ->
      {
        tauCont = x;
        tauArgs = eqNameL a marker pos subArgs subRests parsMarker;
      }
let eqActs eq marker subArgs subRests startAct parsMarker startOuts startInps
           startTests startTaus startSums =
  let outs = ref startOuts in
  let accum = ref startAct
  in
    (for i = 0 to eq.numFnouts - 1 do
       outs :=
         ((eqAct eq.fnouts.(i) marker subArgs subRests (!accum + i)
             parsMarker),
          (!accum + i)) :: !outs
     done;
     accum := !accum + eq.numFnouts;
     for i = 0 to eq.numBnouts - 1 do
       outs :=
         ((eqAct eq.bnouts.(i) marker subArgs subRests (!accum + i)
             parsMarker),
          (!accum + i)) :: !outs
     done;
     accum := !accum + eq.numBnouts;
     let inps = ref startInps
     in
       (for i = 0 to eq.numFninps - 1 do
          inps :=
            ((eqAct eq.fninps.(i) marker subArgs subRests (!accum + i)
                parsMarker),
             (!accum + i)) :: !inps
        done;
        accum := !accum + eq.numFninps;
        for i = 0 to eq.numBninps - 1 do
          inps :=
            ((eqAct eq.bninps.(i) marker subArgs subRests (!accum + i)
                parsMarker),
             (!accum + i)) :: !inps
        done;
        accum := !accum + eq.numBninps;
        let tests = ref startTests
        in
          (for i = 0 to eq.numTests - 1 do
             tests :=
               ((eqTest eq.tests.(i) marker subArgs subRests (!accum + i)
                   parsMarker),
                (!accum + i)) :: !tests
           done;
           accum := !accum + eq.numTests;
           let taus = ref startTaus
           in
             (for i = 0 to eq.numTaus - 1 do
                taus :=
                  ((eqTau eq.taus.(i) marker subArgs subRests (!accum + i)
                      parsMarker),
                   (!accum + i)) :: !taus
              done;
              accum := !accum + eq.numTaus;
              let sums = ref startSums
              in
                (list.iter
                   (fun l ->
                      match l with
                      | (sActs, sTaus, sTests) ->
                          let sumActs = ref []
                          in
                            (list.iter
                               (fun act ->
                                  sumActs :=
                                    (eqAct act marker subArgs subRests 
                                       !accum parsMarker) ::
                                      !sumActs)
                               sActs;
                             let sumTests = ref []
                             in
                               (list.iter
                                  (fun test ->
                                     sumTests :=
                                       (eqTest test marker subArgs subRests
                                          !accum parsMarker) ::
                                         !sumTests)
                                  sTests;
                                let sumTaus = ref []
                                in
                                  (list.iter
                                     (fun tau ->
                                        sumTaus :=
                                          (eqTau tau marker subArgs subRests
                                             !accum parsMarker) ::
                                            !sumTaus)
                                     sTaus;
                                   sums :=
                                     (((!sumActs), (!sumTaus), (!sumTests)),
                                      (!accum)) :: !sums;
                                   incr accum))))
                   eq.sums;
                 (outs, inps, tests, taus, sums))))))
(***) (* Auxiliar functions to handle top level process component creation *)
let createComps outs inps tests taus sums numComps compActs =
  let resFnos = array.create numComps [] in
  let resBnos = array.create numComps [] in
  let resFnis = array.create numComps [] in
  let resBnis = array.create numComps [] in
  let resTests = array.create numComps [] in
  let resTaus = array.create numComps [] in
  let resSums = array.create numComps [] in
  let numOuts = list.length !outs in
  let numInps = list.length !inps in
  let numTests = list.length !tests in
  let numTaus = list.length !taus
  in
    (for i = 0 to (array.length compActs) - 1 do
       if i < (numOuts + numInps)
       then
         (let (act, actPos, out) =
            if i < numOuts
            then
              (let (res, actPos) = list.hd !outs
               in (outs := list.tl !outs; (res, actPos, true)))
            else
              (let (res, actPos) = list.hd !inps
               in (inps := list.tl !inps; (res, actPos, false))) in
          let fn = match act.sub with | bname s -> false | _ -> true
          in
            if fn
            then
              if out
              then
                resFnos.(compActs.(actPos) - 1) <-
                  act :: resFnos.(compActs.(actPos) - 1)
              else
                resFnis.(compActs.(actPos) - 1) <-
                  act :: resFnis.(compActs.(actPos) - 1)
            else
              if out
              then
                resBnos.(compActs.(actPos) - 1) <-
                  act :: resBnos.(compActs.(actPos) - 1)
              else
                resBnis.(compActs.(actPos) - 1) <-
                  act :: resBnis.(compActs.(actPos) - 1))
       else
         if i < ((numOuts + numInps) + numTests)
         then
           (let (test, testPos) = list.hd !tests
            in
              (tests := list.tl !tests;
               resTests.(compActs.(testPos) - 1) <-
                 test :: resTests.(compActs.(testPos) - 1)))
         else
           if i < (((numOuts + numInps) + numTests) + numTaus)
           then
             (let (tau, tauPos) = list.hd !taus
              in
                (taus := list.tl !taus;
                 resTaus.(compActs.(tauPos) - 1) <-
                   tau :: resTaus.(compActs.(tauPos) - 1)))
           else
             (let (sum, sumPos) = list.hd !sums
              in
                (sums := list.tl !sums;
                 resSums.(compActs.(sumPos) - 1) <-
                   sum :: resSums.(compActs.(sumPos) - 1)))
     done;
     (resFnos, resBnos, resFnis, resBnis, resTests, resTaus, resSums))
let eqRests compNames numComps subRests size resNames startPos =
  for i = 0 to size - 1 do
    if compNames.(i + startPos) <> 0
    then
      resNames.(compNames.(i + startPos) - 1) <-
        (bname subRests.(i)) :: resNames.(compNames.(i + startPos) - 1)
    else ()
  done
(***) (*** Creates a top level process from an equation system ***)
let nf2process nf args =
  match nf with
  | (s, f, x, pars) ->
      if isVoidEqvar x
      then
        (let res =
           {
             nComps = 1;
             comps = array.create 1 (ref (nilComponent ()));
             env = nilEnv ();
             fns = nilFns ();
           }
         in ref res)
      else
        (let eq = hashtbl.find !s x in
         let marker =
           array.makeMatrix (countActs eq) (countRests eq) false in
         let subArgs = array.create (list.length args) (fname "") in
         let i = ref 0
         in
           (list.iter (fun s -> (subArgs.(!i) <- fname s; incr i)) args;
            let subRests = freshRests (countRests eq) in
            let (outs, inps, tests, taus, sums) =
              eqActs eq marker subArgs subRests 0 (ref (hashtbl.create 0)) []
                [] [] [] [] in
            let (numComps, compActs, compNames) = comps marker in
            let (fnoL, bnoL, fniL, bniL, idTs, actTs, actSs) =
              createComps outs inps tests taus sums numComps compActs in
            let rL = array.create numComps []
            in
              (eqRests compNames numComps subRests (countRests eq) rL 0;
               let res =
                 {
                   nComps = numComps;
                   comps = array.create numComps (ref (nilComponent ()));
                   env = s;
                   fns = f;
                 }
               in
                 (for i = 0 to numComps - 1 do
                    res.comps.(i) <-
                      ref
                        (newComponent rL.(i) fnoL.(i) bnoL.(i) fniL.(i)
                           bniL.(i) idTs.(i) actTs.(i) actSs.(i))
                  done;
                  ref res))))
(***) (* Auxiliar functions to test_fn *)
let testFnName n arg =
  match n with | fname s -> s = arg | bname s -> false | iname i -> false
let rec testFnNameL l arg =
  match l with
  | [] -> false
  | hd :: tl -> (testFnName hd arg) || (testFnNameL tl arg)
let testFnAux act n fnsub =
  (fnsub && (testFnName act.sub n)) ||
    (((act.t = outType) && (testFnNameL act.obj n)) ||
       (testFnNameL act.args n))
(*** Tests if a process has a determined free name ***)
let testFn p n =
  let i = ref 0 in
  let j = ref 0 in
  let res = ref false
  in
    (while (not !res) && (!i < !p.nComps) do j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).nfnouts) do
         res := testFnAux !(!p.comps.(!i)).fnOuts.(!j) n true;
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns) !(!p.comps.(!i)).fnOuts.(!j).cont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).nfninps) do
         res := testFnAux !(!p.comps.(!i)).fnInps.(!j) n true;
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns) !(!p.comps.(!i)).fnInps.(!j).cont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).nbnouts) do
         res := testFnAux !(!p.comps.(!i)).bnOuts.(!j) n false;
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns) !(!p.comps.(!i)).bnOuts.(!j).cont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).nbninps) do
         res := testFnAux !(!p.comps.(!i)).bnInps.(!j) n false;
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns) !(!p.comps.(!i)).bnInps.(!j).cont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).ntests) do
         res := testFnName !(!p.comps.(!i)).idTests.(!j).idl n;
         res := !res || (testFnName !(!p.comps.(!i)).idTests.(!j).idr n);
         res := !res || (testFnNameL !(!p.comps.(!i)).idTests.(!j).targs n);
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns) !(!p.comps.(!i)).idTests.(!j).tcont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       j := 0;
       while (not !res) && (!j < !(!p.comps.(!i)).ntaus) do
         res := testFnNameL !(!p.comps.(!i)).actTaus.(!j).tauArgs n;
         res :=
           !res ||
             (try
                let fnL =
                  hashtbl.find !(!p.fns)
                    !(!p.comps.(!i)).actTaus.(!j).tauCont
                in list.mem n !fnL
              with | notFound -> false);
         incr j done;
       (try
          list.iter
            (fun sum ->
               match sum with
               | (acts, taus, tests) ->
                   (list.iter
                      (fun act ->
                         (if !res then raise foundFn else ();
                          res := !res || (testFnAux act n true);
                          res :=
                            !res ||
                              (try
                                 let fnL = hashtbl.find !(!p.fns) act.cont
                                 in list.mem n !fnL
                               with | notFound -> false)))
                      acts;
                    list.iter
                      (fun tau ->
                         (if !res then raise foundFn else ();
                          res := !res || (testFnNameL tau.tauArgs n);
                          res :=
                            !res ||
                              (try
                                 let fnL = hashtbl.find !(!p.fns) tau.tauCont
                                 in list.mem n !fnL
                               with | notFound -> false)))
                      taus;
                    list.iter
                      (fun test ->
                         (if !res then raise foundFn else ();
                          res := testFnName test.idl n;
                          res := !res || (testFnName test.idr n);
                          res := !res || (testFnNameL test.targs n);
                          res :=
                            !res ||
                              (try
                                 let fnL = hashtbl.find !(!p.fns) test.tcont
                                 in list.mem n !fnL
                               with | notFound -> false)))
                      tests))
            !(!p.comps.(!i)).actSums
        with | foundFn -> ignore 0);
       incr i done;
     !res)
(***) (* Auxiliar functions to free_names *)
let freeName n h res =
  match n with
  | fname s ->
      if not (hashtbl.mem !h s)
      then (hashtbl.add !h s 0; res := s :: !res)
      else ()
  | _ -> ignore n
let rec freeNameL l h res =
  match l with
  | [] -> ignore l
  | hd :: tl -> (freeName hd h res; freeNameL tl h res)
let rec fnEqsAux l h res =
  match l with
  | [] -> ignore l
  | hd :: tl ->
      (if not (hashtbl.mem !h hd)
       then (hashtbl.add !h hd 0; res := hd :: !res)
       else ();
       fnEqsAux tl h res)
let fnEqs p c h res =
  try let fnL = hashtbl.find !(!p.fns) c in fnEqsAux !fnL h res
  with | notFound -> ignore c
(*** Returns the set of free names of a process ***)
let freeNames p =
  let h = ref (hashtbl.create 100) in
  let res = ref []
  in
    (for i = 0 to !p.nComps - 1 do
       for j = 0 to !(!p.comps.(i)).nfnouts - 1 do
         freeName !(!p.comps.(i)).fnOuts.(j).sub h res;
         freeNameL !(!p.comps.(i)).fnOuts.(j).obj h res;
         freeNameL !(!p.comps.(i)).fnOuts.(j).args h res;
         fnEqs p !(!p.comps.(i)).fnOuts.(j).cont h res
       done;
       for j = 0 to !(!p.comps.(i)).nfninps - 1 do
         freeName !(!p.comps.(i)).fnInps.(j).sub h res;
         freeNameL !(!p.comps.(i)).fnInps.(j).args h res;
         fnEqs p !(!p.comps.(i)).fnInps.(j).cont h res
       done;
       for j = 0 to !(!p.comps.(i)).nbnouts - 1 do
         freeNameL !(!p.comps.(i)).bnOuts.(j).obj h res;
         freeNameL !(!p.comps.(i)).bnOuts.(j).args h res;
         fnEqs p !(!p.comps.(i)).bnOuts.(j).cont h res
       done;
       for j = 0 to !(!p.comps.(i)).nbninps - 1 do
         freeNameL !(!p.comps.(i)).bnInps.(j).args h res;
         fnEqs p !(!p.comps.(i)).bnInps.(j).cont h res
       done;
       for j = 0 to !(!p.comps.(i)).ntests - 1 do
         freeName !(!p.comps.(i)).idTests.(j).idl h res;
         freeName !(!p.comps.(i)).idTests.(j).idr h res;
         freeNameL !(!p.comps.(i)).idTests.(j).targs h res;
         fnEqs p !(!p.comps.(i)).idTests.(j).tcont h res
       done;
       for j = 0 to !(!p.comps.(i)).ntaus - 1 do
         freeNameL !(!p.comps.(i)).actTaus.(j).tauArgs h res;
         fnEqs p !(!p.comps.(i)).actTaus.(j).tauCont h res
       done;
       list.iter
         (fun sum ->
            match sum with
            | (acts, taus, tests) ->
                (list.iter
                   (fun act ->
                      (freeName act.sub h res;
                       if act.t = outType
                       then freeNameL act.obj h res
                       else ();
                       freeNameL act.args h res;
                       fnEqs p act.cont h res))
                   acts;
                 list.iter
                   (fun tau ->
                      (freeNameL tau.tauArgs h res;
                       fnEqs p tau.tauCont h res))
                   taus;
                 list.iter
                   (fun test ->
                      (freeName test.idl h res;
                       freeName test.idr h res;
                       freeNameL test.targs h res;
                       fnEqs p test.tcont h res))
                   tests))
         !(!p.comps.(i)).actSums
     done;
     !res)
(***) (*** Returns the number of components of a process ***)
let numComps p = !p.nComps (***) (*** Determines if a process is empty ***)
let emptyProc p =
  ((numComps p) = 1) &&
    ((!(!p.comps.(0)).nrests = 0) &&
       ((!(!p.comps.(0)).nfnouts = 0) &&
          ((!(!p.comps.(0)).nfninps = 0) &&
             ((!(!p.comps.(0)).nbnouts = 0) &&
                ((!(!p.comps.(0)).nbninps = 0) &&
                   ((!(!p.comps.(0)).ntests = 0) &&
                      ((!(!p.comps.(0)).ntaus = 0) &&
                         (!(!p.comps.(0)).nsums = 0))))))))
(***) (* Auxiliar function to total_acts *)
let countActsComp p pos =
  (((((!(!p.comps.(pos)).nfnouts + !(!p.comps.(pos)).nfninps) +
        !(!p.comps.(pos)).nbnouts)
       + !(!p.comps.(pos)).nbninps)
      + !(!p.comps.(pos)).ntests)
     + !(!p.comps.(pos)).ntaus)
    + !(!p.comps.(pos)).nsums
(*** Returns the number of actions of a process ***)
let totalActs p =
  let res = ref 0
  in
    (for i = 0 to !p.nComps - 1 do res := !res + (countActsComp p i) done;
     !res)
(***) (*** Builds two processes by separating an existing one ***)
let extractComps p is size dim1 dim2 =
  let p1 =
    {
      nComps = dim1;
      comps = array.create dim1 (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    } in
  let p2 =
    {
      nComps = dim2;
      comps = array.create dim2 (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    } in
  let j = ref 0 in
  let k = ref 0
  in
    (for i = 0 to size - 1 do
       if list.mem i is
       then (p1.comps.(!j) <- !p.comps.(i); incr j)
       else (p2.comps.(!k) <- !p.comps.(i); incr k)
     done;
     ((ref p1), (ref p2)))
(***)
(*** Builds a pair of processes by composing with the empty process ***)
let compEmpty p left =
  let empty =
    {
      nComps = 1;
      comps = array.create 1 (ref (nilComponent ()));
      env = nilEnv ();
      fns = nilFns ();
    }
  in if left then ((ref empty), p) else (p, (ref empty))
(***) (*** Returns the number of restrictions of a process component ***)
let nrestsComp p pos = !(!p.comps.(pos)).nrests (***)
(*** Finds a component that holds restrictions ***)
let findRes p pos =
  let found = ref false in
  let i = ref pos in
  let size = numComps p
  in
    (while (!i < size) && (not !found) do
       if !(!p.comps.(!i)).nrests > 0 then found := true else incr i done;
     !i)
(***) (* Auxiliar functions to handle process updates *)
let instantiateProc p numComps compNum rL fnoL bnoL fniL bniL idTs actTs
                    actSs =
  let size = (!p.nComps + numComps) - 1 in
  let res =
    {
      nComps = size;
      comps = array.create size (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    }
  in
    (for i = 0 to size - 1 do
       if i < compNum
       then res.comps.(i) <- !p.comps.(i)
       else
         if i < (!p.nComps - 1)
         then res.comps.(i) <- !p.comps.(i + 1)
         else
           res.comps.(i) <-
             ref
               (newComponent rL.(i - (!p.nComps - 1))
                  fnoL.(i - (!p.nComps - 1)) bnoL.(i - (!p.nComps - 1))
                  fniL.(i - (!p.nComps - 1)) bniL.(i - (!p.nComps - 1))
                  idTs.(i - (!p.nComps - 1)) actTs.(i - (!p.nComps - 1))
                  actSs.(i - (!p.nComps - 1)))
     done;
     ref res)
let instantiateProc2 p numComps comp1 comp2 rL fnoL bnoL fniL bniL idTs actTs
                     actSs =
  let size = (!p.nComps + numComps) - 2 in
  let res =
    {
      nComps = size;
      comps = array.create size (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    } in
  let minComp = min comp1 comp2 in
  let maxComp = max comp1 comp2
  in
    (for i = 0 to size - 1 do
       if i < minComp
       then res.comps.(i) <- !p.comps.(i)
       else
         if i < (maxComp - 1)
         then res.comps.(i) <- !p.comps.(i + 1)
         else
           if i < (!p.nComps - 2)
           then res.comps.(i) <- !p.comps.(i + 2)
           else
             res.comps.(i) <-
               ref
                 (newComponent rL.(i - (!p.nComps - 2))
                    fnoL.(i - (!p.nComps - 2)) bnoL.(i - (!p.nComps - 2))
                    fniL.(i - (!p.nComps - 2)) bniL.(i - (!p.nComps - 2))
                    idTs.(i - (!p.nComps - 2)) actTs.(i - (!p.nComps - 2))
                    actSs.(i - (!p.nComps - 2)))
     done;
     ref res)
(* Restricted name update identification *)
let restMarker comp startRest pos rmarker =
  let count = ref startRest
  in
    for i = 0 to !comp.nrests - 1 do
      if i <> pos
      then
        (hashtbl.add !rmarker (getString !comp.rests.(i)) !count; incr count)
      else ()
    done
(* Action update handling *)
let procName arg oldn newn marker rmarker pos =
  match arg with
  | bname s ->
      if s = oldn
      then fname newn
      else
        (let k = hashtbl.find !rmarker s
         in (marker.(pos).(k) <- true; bname s))
  | fname s -> fname s
  | iname i -> iname i
let rec procNameL l oldn newn marker rmarker pos =
  match l with
  | [] -> []
  | hd :: tl ->
      (procName hd oldn newn marker rmarker pos) ::
        (procNameL tl oldn newn marker rmarker pos)
let procAct act oldn newn marker rmarker pos fnsub =
  {
    t = act.t;
    sub =
      if fnsub
      then act.sub
      else procName act.sub oldn newn marker rmarker pos;
    obj =
      if act.t = outType
      then procNameL act.obj oldn newn marker rmarker pos
      else act.obj;
    cont = act.cont;
    args = procNameL act.args oldn newn marker rmarker pos;
  }
let procTest test oldn newn marker rmarker pos =
  {
    tst = test.tst;
    idl = procName test.idl oldn newn marker rmarker pos;
    idr = procName test.idr oldn newn marker rmarker pos;
    tcont = test.tcont;
    targs = procNameL test.targs oldn newn marker rmarker pos;
  }
let procTau tau oldn newn marker rmarker pos =
  {
    tauCont = tau.tauCont;
    tauArgs = procNameL tau.tauArgs oldn newn marker rmarker pos;
  }
let procActs comp marker oldn revn startAct rmarker startOuts startInps
             startTests startTaus startSums inpInd outInd fn ti taui =
  let outs = ref startOuts in
  let accum = ref startAct
  in
    (for i = 0 to !comp.nfnouts - 1 do
       if (not fn) || (outInd <> i)
       then
         outs :=
           ((procAct !comp.fnOuts.(i) oldn revn marker rmarker (!accum + i)
               true),
            (!accum + i)) :: !outs
       else decr accum
     done;
     accum := !accum + !comp.nfnouts;
     for i = 0 to !comp.nbnouts - 1 do
       if fn || (outInd <> i)
       then
         outs :=
           ((procAct !comp.bnOuts.(i) oldn revn marker rmarker (!accum + i)
               false),
            (!accum + i)) :: !outs
       else decr accum
     done;
     accum := !accum + !comp.nbnouts;
     let inps = ref startInps
     in
       (for i = 0 to !comp.nfninps - 1 do
          if (not fn) || (inpInd <> i)
          then
            inps :=
              ((procAct !comp.fnInps.(i) oldn revn marker rmarker
                  (!accum + i) true),
               (!accum + i)) :: !inps
          else decr accum
        done;
        accum := !accum + !comp.nfninps;
        for i = 0 to !comp.nbninps - 1 do
          if fn || (inpInd <> i)
          then
            inps :=
              ((procAct !comp.bnInps.(i) oldn revn marker rmarker
                  (!accum + i) false),
               (!accum + i)) :: !inps
          else decr accum
        done;
        accum := !accum + !comp.nbninps;
        let tests = ref startTests
        in
          (for i = 0 to !comp.ntests - 1 do
             if i <> ti
             then
               tests :=
                 ((procTest !comp.idTests.(i) oldn revn marker rmarker
                     (!accum + i)),
                  (!accum + i)) :: !tests
             else decr accum
           done;
           accum := !accum + !comp.ntests;
           let taus = ref startTaus
           in
             (for i = 0 to !comp.ntaus - 1 do
                if i <> taui
                then
                  taus :=
                    ((procTau !comp.actTaus.(i) oldn revn marker rmarker
                        (!accum + i)),
                     (!accum + i)) :: !taus
                else decr accum
              done;
              accum := !accum + !comp.ntaus;
              let sums = ref startSums in
              let inpSumI =
                ref (if fn then !comp.nfninps else !comp.nbninps) in
              let outSumI =
                ref (if fn then !comp.nfnouts else !comp.nbnouts) in
              let testSumI = ref !comp.ntests in
              let tauSumI = ref !comp.ntaus
              in
                (list.iter
                   (fun l ->
                      match l with
                      | (acts, taus, tests) ->
                          if
                            ((inpInd >= !inpSumI) &&
                               (inpInd < (!inpSumI + (list.length acts))))
                              ||
                              (((outInd >= !outSumI) &&
                                  (outInd < (!outSumI + (list.length acts))))
                                 ||
                                 (((taui >= !tauSumI) &&
                                     (taui < (!tauSumI + (list.length taus))))
                                    ||
                                    ((ti >= !testSumI) &&
                                       (ti <
                                          (!testSumI + (list.length tests))))))
                          then
                            (inpSumI := !inpSumI + (list.length acts);
                             outSumI := !outSumI + (list.length acts);
                             tauSumI := !tauSumI + (list.length taus);
                             testSumI := !testSumI + (list.length tests))
                          else
                            (inpSumI := !inpSumI + (list.length acts);
                             outSumI := !outSumI + (list.length acts);
                             tauSumI := !tauSumI + (list.length taus);
                             testSumI := !testSumI + (list.length tests);
                             (let newActs = ref []
                              in
                                (list.iter
                                   (fun act ->
                                      newActs :=
                                        (procAct act oldn revn marker rmarker
                                           !accum false) ::
                                          !newActs)
                                   acts;
                                 let newTaus = ref []
                                 in
                                   (list.iter
                                      (fun tau ->
                                         newTaus :=
                                           (procTau tau oldn revn marker
                                              rmarker !accum) ::
                                             !newTaus)
                                      taus;
                                    let newTests = ref []
                                    in
                                      (list.iter
                                         (fun test ->
                                            newTests :=
                                              (procTest test oldn revn marker
                                                 rmarker !accum) ::
                                                !newTests)
                                         tests;
                                       sums :=
                                         (((!newActs), (!newTaus),
                                           (!newTests)),
                                          (!accum)) :: !sums;
                                       incr accum))))))
                   !comp.actSums;
                 (outs, inps, tests, taus, sums))))))
(* Restring name update handling *)
let procRests compNames numComps comp pos resNames startPos =
  for i = 0 to !comp.nrests - 1 do
    let newRestPos =
      if i < pos then i else if i < (!comp.nrests - 1) then i + 1 else (-1)
    in
      if (newRestPos <> (-1)) && (compNames.(i + startPos) <> 0)
      then
        resNames.(compNames.(i + startPos) - 1) <-
          (bname (getString !comp.rests.(newRestPos))) ::
            resNames.(compNames.(i + startPos) - 1)
      else ()
  done
(***)
(*** Returns a process where a restricted name revelation has taken place ***)
let revComps p compNum restNum revn =
  let comp = !p.comps.(compNum) in
  let oldn = getString !comp.rests.(restNum) in
  let marker =
    array.makeMatrix (countActsComp p compNum) (!comp.nrests - 1) false in
  let rmarker = ref (hashtbl.create !comp.nrests)
  in
    (restMarker comp 0 restNum rmarker;
     let (outs, inps, tests, taus, sums) =
       procActs comp marker oldn revn 0 rmarker [] [] [] [] [] (-1) (-1) true
         (-1) (-1) in
     let (numComps, compActs, compNames) = comps marker in
     let (fnoL, bnoL, fniL, bniL, idTs, actTs, actSs) =
       createComps outs inps tests taus sums numComps compActs in
     let rL = array.create numComps []
     in
       (procRests compNames numComps comp restNum rL 0;
        instantiateProc p numComps compNum rL fnoL bnoL fniL bniL idTs actTs
          actSs))
(***) (* Auxiliar functions to find_label *)
let matchName s n =
  match n with | bname x -> false | fname x -> x = s | iname i -> false
let rec matchList sl nl marker i h =
  match nl with
  | [] -> (match sl with | [] -> (true, []) | s :: sltl -> (false, []))
  | bname n :: nltl ->
      if marker.(i)
      then
        (match sl with
         | [] -> (false, [])
         | s :: sltl ->
             let (res, resL) = matchList sltl nltl marker (i + 1) h
             in
               if res
               then
                 if not (hashtbl.mem !h n)
                 then
                   (let ns = if s = "_" then freshName () else s
                    in (hashtbl.add !h n ns; (true, ((ns, n) :: resL))))
                 else
                   if (hashtbl.find !h n) = s
                   then (true, resL)
                   else (false, [])
               else (false, []))
      else (false, [])
  | fname n :: nltl ->
      (match sl with
       | [] -> (false, [])
       | s :: sltl ->
           let (res, resL) = matchList sltl nltl marker (i + 1) h
           in if res then (((s = n) || (s = "_")), resL) else (false, []))
  | iname i :: nltl -> (false, [])
let matchLab t sub obj act marker =
  if t <> act.t
  then (false, [])
  else
    if t = inpType
    then
      (((matchName sub act.sub) &&
          ((list.length obj) = (list.length act.obj))),
       [])
    else
      if
        (matchName sub act.sub) &&
          ((list.length obj) = (list.length act.obj))
      then
        matchList obj act.obj marker 0
          (ref (hashtbl.create (list.length obj)))
      else (false, [])
(***) (*** Finds an action given the label and a starting point ***)
let findLabel p lab comp ind marker =
  let notFound = ref true in
  let reveals = ref [] in
  let size1 = !p.nComps in
  let curComp = ref comp in
  let curInd = ref ind
  in
    ((match lab with
      | (t, sub, obj) ->
          while (!curComp < size1) && !notFound do
            let (size2, acts) =
              if t = inpType
              then
                ((!(!p.comps.(!curComp)).nfninps),
                 (ref !(!p.comps.(!curComp)).fnInps))
              else
                ((!(!p.comps.(!curComp)).nfnouts),
                 (ref !(!p.comps.(!curComp)).fnOuts))
            in
              (while (!curInd < size2) && !notFound do
                 (let (res, resL) = matchLab t sub obj !acts.(!curInd) marker
                  in
                    if res
                    then (notFound := false; reveals := resL)
                    else incr curInd)
                 done;
               if !notFound
               then
                 (let sumInd = ref (!curInd - size2)
                  in
                    try
                      list.iter
                        (fun sum ->
                           match sum with
                           | (acts, taus, tests) ->
                               list.iter
                                 (fun act ->
                                    if !sumInd = 0
                                    then
                                      (let (res, resL) =
                                         matchLab t sub obj act marker
                                       in
                                         if res
                                         then
                                           (notFound := false;
                                            reveals := resL;
                                            raise found)
                                         else incr curInd)
                                    else decr sumInd)
                                 acts)
                        !(!p.comps.(!curComp)).actSums
                    with | found -> ignore 1)
               else ();
               if !notFound then (incr curComp; curInd := 0) else ())
            done);
     ((!notFound), (!curComp), (!curInd), (!reveals)))
(***) (*** Finds a restricted name component ***)
let findRest p comp n =
  let res = ref (-1) in
  let i = ref 0
  in
    (while (!res = (-1)) && (!i < !(!p.comps.(comp)).nrests) do
       if (getString !(!p.comps.(comp)).rests.(!i)) = n
       then res := !i
       else incr i done;
     !res)
(***) (*** Finds a component that holds restrictions ***)
let findRests p =
  let res = ref (-1) in
  let i = ref 0
  in
    (while (!res = (-1)) && (!i < !p.nComps) do
       if !(!p.comps.(!i)).nrests > 0 then res := !i else incr i done;
     !res)
(***) (* Auxiliar functions to react_label *)
let cutComp p pos =
  let res =
    {
      nComps = !p.nComps - 1;
      comps = array.create (!p.nComps - 1) (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    }
  in
    (for i = 0 to !p.nComps - 2 do
       if i < pos
       then res.comps.(i) <- !p.comps.(i)
       else res.comps.(i) <- !p.comps.(i + 1)
     done;
     ref res)
let cutTwoComps p pos1 pos2 =
  let res =
    {
      nComps = !p.nComps - 2;
      comps = array.create (!p.nComps - 2) (ref (nilComponent ()));
      env = !p.env;
      fns = !p.fns;
    } in
  let minPos = min pos1 pos2 in
  let maxPos = max pos1 pos2
  in
    (for i = 0 to !p.nComps - 3 do
       if i < minPos
       then res.comps.(i) <- !p.comps.(i)
       else
         if i < (maxPos - 1)
         then res.comps.(i) <- !p.comps.(i + 1)
         else res.comps.(i) <- !p.comps.(i + 2)
     done;
     ref res)
let rec reactLabelArgs inpL subArgs =
  let aux = array.ofList inpL
  in
    for i = 0 to (array.length subArgs) - 1 do
      match subArgs.(i) with
      | iname k -> subArgs.(i) <- fname aux.(k)
      | _ -> ignore i
    done
(* Handles the process update after the transition *)
let reactLabelAux p pos eq act args ind inp =
  let comp = !p.comps.(pos) in
  let marker =
    array.makeMatrix (((countActs eq) + (countActsComp p pos)) - 1)
      ((countRests eq) + !comp.nrests) false in
  let subArgs = array.ofList act.args
  in
    (reactLabelArgs args subArgs;
     let subRests = freshRests (countRests eq) in
     let rmarker = ref (hashtbl.create !comp.nrests)
     in
       (restMarker comp (countRests eq) !comp.nrests rmarker;
        let (os, is, tsts, ts, ss) =
          eqActs eq marker subArgs subRests 0 rmarker [] [] [] [] [] in
        let freshn = freshName () in
        let (inpInd, outInd) = if inp then (ind, (-1)) else ((-1), ind) in
        let (outs, inps, tests, taus, sums) =
          procActs comp marker freshn freshn (countActs eq) rmarker !os 
            !is !tsts !ts !ss inpInd outInd true (-1) (-1) in
        let (numComps, compActs, compNames) = comps marker in
        let (fnoL, bnoL, fniL, bniL, idTs, actTs, actSs) =
          createComps outs inps tests taus sums numComps compActs in
        let rL = array.create numComps []
        in
          (eqRests compNames numComps subRests (countRests eq) rL 0;
           procRests compNames numComps comp !comp.nrests rL (countRests eq);
           instantiateProc p numComps pos rL fnoL bnoL fniL bniL idTs actTs
             actSs)))
(***)
let rec getPosSum sum ind i =
  match sum with
  | [] -> raise error
  | hd :: tl -> if ind = i then hd else getPosSum tl ind (i + 1)
let rec getActSums sums ind i =
  match sums with
  | [] -> raise error
  | (acts, taus, tests) :: tl ->
      if ind < ((list.length acts) + i)
      then getPosSum acts ind i
      else getActSums tl ind (i + (list.length acts))
(***)
(*** Returns a process where a transition on a given label has taken place ***)
let reactLabel lab pos ind p =
  match lab with
  | (t, sub, obj) ->
      let (act, inp) =
        if t = inpType
        then
          if ind < !(!p.comps.(pos)).nfninps
          then ((!(!p.comps.(pos)).fnInps.(ind)), true)
          else
            ((getActSums !(!p.comps.(pos)).actSums
                (ind - !(!p.comps.(pos)).nfninps) 0),
             true)
        else
          if ind < !(!p.comps.(pos)).nfnouts
          then ((!(!p.comps.(pos)).fnOuts.(ind)), false)
          else
            ((getActSums !(!p.comps.(pos)).actSums
                (ind - !(!p.comps.(pos)).nfnouts) 0),
             false)
      in
        if isVoidEqvar act.cont
        then
          if !p.nComps = 1
          then
            ref
              {
                nComps = 1;
                comps = array.create 1 (ref (nilComponent ()));
                env = nilEnv ();
                fns = nilFns ();
              }
          else cutComp p pos
        else
          (let eq = hashtbl.find !(!p.env) act.cont
           in reactLabelAux p pos eq act obj ind inp)
(***) (* Auxiliar functions to find_fn_tau and find_bn_tau *)
let matchTauName n1 n2 =
  match n1 with
  | bname x ->
      (match n2 with | bname y -> x = y | fname y -> false | iname i -> false)
  | fname x ->
      (match n2 with | bname y -> false | fname y -> x = y | iname i -> false)
  | iname i -> false
let matchTau inp out =
  (inp.t = inpType) &&
    ((out.t = outType) &&
       ((matchTauName inp.sub out.sub) &&
          ((list.length inp.obj) = (list.length out.obj))))
let isFnAct act = match act.sub with | fname s -> true | _ -> false (***)
(*** Finds a synchronization in a free name ***)
let findFnTau p inpc inpi outc outi =
  let notFound = ref true in
  let inpComp = ref inpc in
  let inpInd = ref inpi in
  let outComp = ref outc in
  let outInd = ref outi
  in
    (while (!inpComp < !p.nComps) && !notFound do
       while (!inpInd < !(!p.comps.(!inpComp)).nfninps) && !notFound do
         while (!outComp < !p.nComps) && !notFound do
           while (!outInd < !(!p.comps.(!outComp)).nfnouts) && !notFound do
             if
               matchTau !(!p.comps.(!inpComp)).fnInps.(!inpInd)
                 !(!p.comps.(!outComp)).fnOuts.(!outInd)
             then notFound := false
             else incr outInd done;
           if !notFound
           then
             (let outSumInd = ref (!outInd - !(!p.comps.(!outComp)).nfnouts)
              in
                try
                  list.iter
                    (fun sum ->
                       match sum with
                       | (acts, taus, tests) ->
                           list.iter
                             (fun out ->
                                if !outSumInd = 0
                                then
                                  (if (isFnAct out) && (out.t = outType)
                                   then
                                     if
                                       matchTau
                                         !(!p.comps.(!inpComp)).fnInps.
                                           (!inpInd)
                                         out
                                     then (notFound := false; raise found)
                                     else ()
                                   else ();
                                   incr outInd)
                                else decr outSumInd)
                             acts)
                    !(!p.comps.(!outComp)).actSums
                with | found -> ignore 1)
           else (); if !notFound then (incr outComp; outInd := 0) else ()
           done;
         if !notFound then (incr inpInd; outComp := 0; outInd := 0) else ()
         done;
       if !notFound
       then
         (let inpSumInd = ref (!inpInd - !(!p.comps.(!inpComp)).nfninps) in
          let inpSumPos = ref 0
          in
            try
              list.iter
                (fun inpSum ->
                   match inpSum with
                   | (acts, taus, tests) ->
                       (list.iter
                          (fun inp ->
                             if !inpSumInd = 0
                             then
                               (if (isFnAct inp) && (inp.t = inpType)
                                then
                                  while (!outComp < !p.nComps) && !notFound
                                    do
                                    while
                                      (!outInd <
                                         !(!p.comps.(!outComp)).nfnouts)
                                        && !notFound
                                      do
                                      if
                                        matchTau inp
                                          !(!p.comps.(!outComp)).fnOuts.
                                            (!outInd)
                                      then (notFound := false; raise found)
                                      else incr outInd done;
                                    if !notFound
                                    then
                                      (let outSumInd =
                                         ref
                                           (!outInd -
                                              !(!p.comps.(!outComp)).nfnouts) in
                                       let outSumPos = ref 0
                                       in
                                         list.iter
                                           (fun outSum ->
                                              ((match outSum with
                                                | (acts, taus, tests) ->
                                                    if
                                                      (!inpComp <> !outComp)
                                                        ||
                                                        (!inpSumPos <>
                                                           !outSumPos)
                                                    then
                                                      list.iter
                                                        (fun out ->
                                                           if !outSumInd = 0
                                                           then
                                                             (if
                                                                (isFnAct out)
                                                                  &&
                                                                  (out.t =
                                                                    outType)
                                                              then
                                                                if
                                                                  matchTau
                                                                    inp out
                                                                then
                                                                  (notFound :=
                                                                    false;
                                                                   raise
                                                                    found)
                                                                else ()
                                                              else ();
                                                              incr outInd)
                                                           else
                                                             decr outSumInd)
                                                        acts
                                                    else
                                                      (let outSumLen 
                                                         = list.length acts
                                                       in
                                                         if
                                                           !outSumInd >=
                                                             outSumLen
                                                         then
                                                           outSumInd :=
                                                             !outSumInd -
                                                               outSumLen
                                                         else
                                                           (outSumInd := 0;
                                                            outInd :=
                                                              !outInd +
                                                                outSumLen)));
                                               incr outSumPos))
                                           !(!p.comps.(!outComp)).actSums)
                                    else ();
                                    if !notFound
                                    then (incr outComp; outInd := 0)
                                    else () done
                                else ();
                                incr inpInd;
                                outComp := 0;
                                outInd := 0)
                             else decr inpSumInd)
                          acts;
                        incr inpSumPos))
                !(!p.comps.(!inpComp)).actSums
            with | found -> ignore 1)
       else ();
       if !notFound
       then (incr inpComp; inpInd := 0; outComp := 0; outInd := 0)
       else () done;
     ((!notFound), (!inpComp), (!inpInd), (!outComp), (!outInd)))
(***) (*** Finds a synchronization in a bound name ***)
let findBnTau p c inpi outi =
  let notFound = ref true in
  let comp = ref c in
  let inpInd = ref inpi in
  let outInd = ref outi
  in
    (while (!comp < !p.nComps) && !notFound do
       while (!inpInd < !(!p.comps.(!comp)).nbninps) && !notFound do
         while (!outInd < !(!p.comps.(!comp)).nbnouts) && !notFound do
           if
             matchTau !(!p.comps.(!comp)).bnInps.(!inpInd)
               !(!p.comps.(!comp)).bnOuts.(!outInd)
           then notFound := false
           else incr outInd done;
         if !notFound
         then
           (let outSumInd = ref (!outInd - !(!p.comps.(!comp)).nbnouts)
            in
              try
                list.iter
                  (fun sum ->
                     match sum with
                     | (acts, taus, tests) ->
                         list.iter
                           (fun out ->
                              if !outSumInd = 0
                              then
                                (if (not (isFnAct out)) && (out.t = outType)
                                 then
                                   if
                                     matchTau
                                       !(!p.comps.(!comp)).bnInps.(!inpInd)
                                       out
                                   then (notFound := false; raise found)
                                   else ()
                                 else ();
                                 incr outInd)
                              else decr outSumInd)
                           acts)
                  !(!p.comps.(!comp)).actSums
              with | found -> ignore 1)
         else (); if !notFound then (incr inpInd; outInd := 0) else () done;
       if !notFound
       then
         (let inpSumInd = ref (!inpInd - !(!p.comps.(!comp)).nbninps) in
          let inpSumPos = ref 0
          in
            try
              list.iter
                (fun inpSum ->
                   match inpSum with
                   | (acts, taus, tests) ->
                       (list.iter
                          (fun inp ->
                             if !inpSumInd = 0
                             then
                               (if (not (isFnAct inp)) && (inp.t = inpType)
                                then
                                  while
                                    (!outInd < !(!p.comps.(!comp)).nbnouts)
                                      && !notFound
                                    do
                                    if
                                      matchTau inp
                                        !(!p.comps.(!comp)).bnOuts.(!outInd)
                                    then (notFound := false; raise found)
                                    else incr outInd done
                                else ();
                                if
                                  (not (isFnAct inp)) &&
                                    ((inp.t = inpType) && !notFound)
                                then
                                  (let outSumInd =
                                     ref
                                       (!outInd - !(!p.comps.(!comp)).nbnouts) in
                                   let outSumPos = ref 0
                                   in
                                     list.iter
                                       (fun outSum ->
                                          ((match outSum with
                                            | (acts, taus, tests) ->
                                                if !inpSumPos <> !outSumPos
                                                then
                                                  list.iter
                                                    (fun out ->
                                                       if !outSumInd = 0
                                                       then
                                                         (if
                                                            (not
                                                               (isFnAct out))
                                                              &&
                                                              (out.t =
                                                                 outType)
                                                          then
                                                            if
                                                              matchTau inp
                                                                out
                                                            then
                                                              (notFound :=
                                                                 false;
                                                               raise found)
                                                            else ()
                                                          else ();
                                                          incr outInd)
                                                       else decr outSumInd)
                                                    acts
                                                else
                                                  (let outSumLen =
                                                     list.length acts
                                                   in
                                                     if
                                                       !outSumInd >=
                                                         outSumLen
                                                     then
                                                       outSumInd :=
                                                         !outSumInd -
                                                           outSumLen
                                                     else
                                                       (outSumInd := 0;
                                                        outInd :=
                                                          !outInd + outSumLen)));
                                           incr outSumPos))
                                       !(!p.comps.(!comp)).actSums)
                                else ();
                                incr inpInd;
                                outInd := 0)
                             else decr inpSumInd)
                          acts;
                        incr inpSumPos))
                !(!p.comps.(!comp)).actSums
            with | found -> ignore 1)
       else ();
       if !notFound then (incr comp; inpInd := 0; outInd := 0) else () done;
     ((!notFound), (!comp), (!inpInd), (!outInd)))
(***) (* Auxiliar function to react_tau_aux *)
let rec reactTauArgs inpL subArgs =
  let aux = array.ofList inpL
  in
    for i = 0 to (array.length subArgs) - 1 do
      match subArgs.(i) with
      | iname k -> subArgs.(i) <- aux.(k)
      | _ -> ignore i
    done
(* Handles the process update after the synchronization *)
let reactTauAux p inpC outC inpEq outEq inpAct outAct inpI outI fn =
  let inpComp = !p.comps.(inpC) in
  let outComp = !p.comps.(outC) in
  let (numActs, numRests) =
    if inpC <> outC
    then
      ((((((countActs inpEq) + (countActs outEq)) + (countActsComp p inpC)) +
           (countActsComp p outC))
          - 2),
       ((((countRests inpEq) + (countRests outEq)) + !inpComp.nrests) +
          !outComp.nrests))
    else
      (((((countActs inpEq) + (countActs outEq)) + (countActsComp p inpC)) -
          2),
       (((countRests inpEq) + (countRests outEq)) + !inpComp.nrests)) in
  let marker = array.makeMatrix numActs numRests false in
  let inpSubArgs = array.ofList inpAct.args
  in
    (reactTauArgs outAct.obj inpSubArgs;
     let outSubArgs = array.ofList outAct.args in
     let inpSubRests = freshRests (countRests inpEq) in
     let outSubRests = freshRests (countRests outEq) in
     let rmarker = ref (hashtbl.create (!inpComp.nrests + !outComp.nrests))
     in
       (restMarker inpComp ((countRests inpEq) + (countRests outEq))
          !inpComp.nrests rmarker;
        if inpC <> outC
        then
          restMarker outComp
            (((countRests inpEq) + (countRests outEq)) + !inpComp.nrests)
            !outComp.nrests rmarker
        else ();
        let (os1, is1, tsts1, ts1, ss1) =
          eqActs inpEq marker inpSubArgs inpSubRests 0 rmarker [] [] [] [] []
        in
          (if (countRests outEq) > 0
           then
             (let i = ref ((countRests inpEq) - 1)
              in
                while !i >= 0 do
                  for j = 0 to (countActs inpEq) - 1 do
                    marker.(j).(!i + (countRests outEq)) <- marker.(j).(!i)
                  done; decr i done)
           else ();
           let (os2, is2, tsts2, ts2, ss2) =
             eqActs outEq marker outSubArgs outSubRests (countActs inpEq)
               rmarker !os1 !is1 !tsts1 !ts1 !ss1 in
           let freshn = freshName () in
           let startAct = (countActs inpEq) + (countActs outEq) in
           let (outs, inps, tests, taus, sums) =
             if inpC <> outC
             then
               (let (os3, is3, tsts3, ts3, ss3) =
                  procActs inpComp marker freshn freshn startAct rmarker 
                    !os2 !is2 !tsts2 !ts2 !ss2 inpI (-1) fn (-1) (-1)
                in
                  procActs outComp marker freshn freshn
                    ((startAct + (countActsComp p inpC)) - 1) rmarker 
                    !os3 !is3 !tsts3 !ts3 !ss3 (-1) outI fn (-1) (-1))
             else
               procActs inpComp marker freshn freshn startAct rmarker 
                 !os2 !is2 !tsts2 !ts2 !ss2 inpI outI fn (-1) (-1) in
           let (numComps, compActs, compNames) = comps marker in
           let (fnoL, bnoL, fniL, bniL, idTs, actTs, actSs) =
             createComps outs inps tests taus sums numComps compActs in
           let rL = array.create numComps []
           in
             (eqRests compNames numComps outSubRests (countRests outEq) rL 0;
              eqRests compNames numComps inpSubRests (countRests inpEq) rL
                (countRests outEq);
              let startRests = (countRests outEq) + (countRests inpEq)
              in
                (procRests compNames numComps inpComp !inpComp.nrests rL
                   startRests;
                 if inpC <> outC
                 then
                   (procRests compNames numComps outComp !outComp.nrests rL
                      (startRests + !inpComp.nrests);
                    instantiateProc2 p numComps inpC outC rL fnoL bnoL fniL
                      bniL idTs actTs actSs)
                 else
                   instantiateProc p numComps inpC rL fnoL bnoL fniL bniL
                     idTs actTs actSs)))))
(***)
(*** Returns a process where an internal transition has taken place ***)
let reactTau inpC inpI outC outI fn p =
  let (inpAct, outAct) =
    if fn
    then
      ((if inpI < !(!p.comps.(inpC)).nfninps
        then !(!p.comps.(inpC)).fnInps.(inpI)
        else
          getActSums !(!p.comps.(inpC)).actSums
            (inpI - !(!p.comps.(inpC)).nfninps) 0),
       (if outI < !(!p.comps.(outC)).nfnouts
        then !(!p.comps.(outC)).fnOuts.(outI)
        else
          getActSums !(!p.comps.(outC)).actSums
            (outI - !(!p.comps.(outC)).nfnouts) 0))
    else
      ((if inpI < !(!p.comps.(inpC)).nbninps
        then !(!p.comps.(inpC)).bnInps.(inpI)
        else
          getActSums !(!p.comps.(inpC)).actSums
            (inpI - !(!p.comps.(inpC)).nbninps) 0),
       (if outI < !(!p.comps.(outC)).nbnouts
        then !(!p.comps.(outC)).bnOuts.(outI)
        else
          getActSums !(!p.comps.(outC)).actSums
            (outI - !(!p.comps.(outC)).nbnouts) 0)) in
  let inpEq =
    if isVoidEqvar inpAct.cont
    then nilEq ()
    else hashtbl.find !(!p.env) inpAct.cont in
  let outEq =
    if isVoidEqvar outAct.cont
    then nilEq ()
    else hashtbl.find !(!p.env) outAct.cont in
  let voidConts =
    ((countRests inpEq) = 0) &&
      (((countActs inpEq) = 0) &&
         (((countRests outEq) = 0) && ((countActs outEq) = 0)))
  in
    if
      voidConts &&
        (((!p.nComps = 1) && ((countActsComp p 0) = 2)) ||
           ((!p.nComps = 2) &&
              (((countActsComp p inpC) = 1) && ((countActsComp p outC) = 1))))
    then
      ref
        {
          nComps = 1;
          comps = array.create 1 (ref (nilComponent ()));
          env = nilEnv ();
          fns = nilFns ();
        }
    else
      if voidConts && ((inpC = outC) && ((countActsComp p inpC) = 2))
      then cutComp p inpC
      else
        if
          voidConts &&
            (((countActsComp p inpC) = 1) && ((countActsComp p outC) = 1))
        then cutTwoComps p inpC outC
        else reactTauAux p inpC outC inpEq outEq inpAct outAct inpI outI fn
(***)
(*** Returns the different number of arguments of communications in a given name ***)
let getNumArgs p n =
  let h = hashtbl.create 100 in
  let res = ref []
  in
    (for i = 0 to !p.nComps - 1 do
       for j = 0 to !(!p.comps.(i)).nfninps - 1 do
         (let len = list.length !(!p.comps.(i)).fnInps.(j).obj
          in
            if
              ((getString !(!p.comps.(i)).fnInps.(j).sub) = n) &&
                (not (hashtbl.mem h len))
            then (res := len :: !res; hashtbl.add h len true)
            else ())
       done;
       list.iter
         (fun sum ->
            match sum with
            | (acts, taus, tests) ->
                list.iter
                  (fun inp ->
                     let len = list.length inp.obj
                     in
                       if
                         ((inp.t = inpType) &&
                            ((isFnAct inp) && ((getString inp.sub) = n)))
                           && (not (hashtbl.mem h len))
                       then (res := len :: !res; hashtbl.add h len true)
                       else ())
                  acts)
         !(!p.comps.(i)).actSums
     done;
     !res)
(***) (* Auxiliar function to find_test *)
let matchId test =
  match test.idl with
  | fname s1 ->
      (match test.idr with
       | fname s2 ->
           if test.tst = equations.equalsType then s1 = s2 else not (s1 = s2)
       | _ -> false)
  | bname s1 ->
      (match test.idr with
       | bname s2 ->
           if test.tst = equations.equalsType then s1 = s2 else not (s1 = s2)
       | _ -> false)
  | iname s1 -> false
(*** Finds a test prefix ready to fire ***)
let findTest p comp ind =
  let s1 = !p.nComps in
  let found = ref false in
  let tesComp = ref comp in
  let tesInd = ref ind
  in
    (while (!tesComp < s1) && (not !found) do
       while (!tesInd < !(!p.comps.(!tesComp)).ntests) && (not !found) do
         if matchId !(!p.comps.(!tesComp)).idTests.(!tesInd)
         then found := true
         else incr tesInd done;
       if not !found
       then
         (let sumInd = ref (!tesInd - !(!p.comps.(!tesComp)).ntests)
          in
            try
              list.iter
                (fun sum ->
                   match sum with
                   | (acts, taus, tests) ->
                       list.iter
                         (fun test ->
                            if !sumInd = 0
                            then
                              if matchId test
                              then (found := true; raise found)
                              else incr tesInd
                            else decr sumInd)
                         tests)
                !(!p.comps.(!tesComp)).actSums
            with | found -> ignore 1)
       else (); if not !found then (incr tesComp; tesInd := 0) else () done;
     ((!found), (!tesComp), (!tesInd)))
(***) (* Handles the process update after the ttest firing transition *)
let reactTestAux p pos ind eq test =
  let comp = !p.comps.(pos) in
  let marker =
    array.makeMatrix (((countActs eq) + (countActsComp p pos)) - 1)
      ((countRests eq) + !comp.nrests) false in
  let subArgs = array.ofList test.targs in
  let subRests = freshRests (countRests eq) in
  let rmarker = ref (hashtbl.create !comp.nrests)
  in
    (restMarker comp (countRests eq) !comp.nrests rmarker;
     let (os, is, tsts, ts, ss) =
       eqActs eq marker subArgs subRests 0 rmarker [] [] [] [] [] in
     let freshn = freshName () in
     let (outs, inps, tests, taus, sums) =
       procActs comp marker freshn freshn (countActs eq) rmarker !os 
         !is !tsts !ts !ss (-1) (-1) true ind (-1) in
     let (numComps, compActs, compNames) = comps marker in
     let (fnoL, bnoL, fniL, bniL, idTs, actTs, inpSs) =
       createComps outs inps tests taus sums numComps compActs in
     let rL = array.create numComps []
     in
       (eqRests compNames numComps subRests (countRests eq) rL 0;
        procRests compNames numComps comp !comp.nrests rL (countRests eq);
        instantiateProc p numComps pos rL fnoL bnoL fniL bniL idTs actTs
          inpSs))
(***) (* Auxiliar function to react_test *)
let rec getTestSums sums ind i =
  match sums with
  | [] -> raise error
  | (acts, taus, tests) :: tl ->
      if ind < ((list.length tests) + i)
      then getPosSum tests ind i
      else getTestSums tl ind (i + (list.length tests))
(*** Returns a process where a test firing transition has taken place ***)
let reactTest pos ind p =
  let test =
    if ind < !(!p.comps.(pos)).ntests
    then !(!p.comps.(pos)).idTests.(ind)
    else
      getTestSums !(!p.comps.(pos)).actSums (ind - !(!p.comps.(pos)).ntests)
        0
  in
    if
      (isVoidEqvar test.tcont) &&
        ((!p.nComps = 1) && ((countActsComp p pos) = 1))
    then
      ref
        {
          nComps = 1;
          comps = array.create 1 (ref (nilComponent ()));
          env = nilEnv ();
          fns = nilFns ();
        }
    else
      if (isVoidEqvar test.tcont) && ((countActsComp p pos) = 1)
      then cutComp p pos
      else
        (let testEq =
           if isVoidEqvar test.tcont
           then nilEq ()
           else hashtbl.find !(!p.env) test.tcont
         in reactTestAux p pos ind testEq test)
(***) (*** Finds a tau prefix ***)
let findTauPref p comp ind =
  let s1 = !p.nComps in
  let found = ref false in
  let tauComp = ref comp in
  let tauInd = ref ind
  in
    (while (!tauComp < s1) && (not !found) do
       if (!tauInd < !(!p.comps.(!tauComp)).ntaus) && (not !found)
       then found := true
       else ();
       if not !found
       then
         (let sumInd = ref (!tauInd - !(!p.comps.(!tauComp)).ntaus)
          in
            try
              list.iter
                (fun sum ->
                   match sum with
                   | (acts, taus, tests) ->
                       list.iter
                         (fun tau ->
                            if !sumInd = 0
                            then (found := true; raise found)
                            else decr sumInd)
                         taus)
                !(!p.comps.(!tauComp)).actSums
            with | found -> ignore 1)
       else (); if not !found then (incr tauComp; tauInd := 0) else () done;
     ((!found), (!tauComp), (!tauInd)))
(***) (* Auxiliar function to react_tau_pref *)
let rec getTauSums sums ind i =
  match sums with
  | [] -> raise error
  | (acts, taus, tests) :: tl ->
      if ind < ((list.length taus) + i)
      then getPosSum taus ind i
      else getTauSums tl ind (i + (list.length taus))
(***) (* Handles the process update after the a tau prefix firing *)
let reactTauPrefAux p pos ind eq tau =
  let comp = !p.comps.(pos) in
  let marker =
    array.makeMatrix (((countActs eq) + (countActsComp p pos)) - 1)
      ((countRests eq) + !comp.nrests) false in
  let subArgs = array.ofList tau.tauArgs in
  let subRests = freshRests (countRests eq) in
  let rmarker = ref (hashtbl.create !comp.nrests)
  in
    (restMarker comp (countRests eq) !comp.nrests rmarker;
     let (os, is, tsts, ts, ss) =
       eqActs eq marker subArgs subRests 0 rmarker [] [] [] [] [] in
     let freshn = freshName () in
     let (outs, inps, tests, taus, sums) =
       procActs comp marker freshn freshn (countActs eq) rmarker !os 
         !is !tsts !ts !ss (-1) (-1) true (-1) ind in
     let (numComps, compActs, compNames) = comps marker in
     let (fnoL, bnoL, fniL, bniL, idTs, actTs, inpSs) =
       createComps outs inps tests taus sums numComps compActs in
     let rL = array.create numComps []
     in
       (eqRests compNames numComps subRests (countRests eq) rL 0;
        procRests compNames numComps comp !comp.nrests rL (countRests eq);
        instantiateProc p numComps pos rL fnoL bnoL fniL bniL idTs actTs
          inpSs))
(***)
(*** Returns a process where a tau prefix firing transition has taken place ***)
let reactTauPref pos ind p =
  let tau =
    if ind < !(!p.comps.(pos)).ntaus
    then !(!p.comps.(pos)).actTaus.(ind)
    else
      getTauSums !(!p.comps.(pos)).actSums (ind - !(!p.comps.(pos)).ntaus) 0
  in
    if
      (isVoidEqvar tau.tauCont) &&
        ((!p.nComps = 1) && ((countActsComp p pos) = 1))
    then
      ref
        {
          nComps = 1;
          comps = array.create 1 (ref (nilComponent ()));
          env = nilEnv ();
          fns = nilFns ();
        }
    else
      if (isVoidEqvar tau.tauCont) && ((countActsComp p pos) = 1)
      then cutComp p pos
      else
        (let tauEq =
           if isVoidEqvar tau.tauCont
           then nilEq ()
           else hashtbl.find !(!p.env) tau.tauCont
         in reactTauPrefAux p pos ind tauEq tau)
(***) (*** Handles bound name output revelation ***)
let findNewPos p oldNcomps oldInd =
  let resComp = ref oldNcomps in
  let resInd = ref oldInd in
  let i = ref (oldNcomps - 1) in
  let found = ref false
  in
    (while (!i < !p.nComps) && (not !found) do
       if !resInd < !(!p.comps.(!i)).nfnouts
       then (found := true; resComp := !i)
       else ();
       (let indAux = ref !resInd
        in
          (if not !found
           then
             (try
                list.iter
                  (fun sum ->
                     match sum with
                     | (acts, taus, tests) ->
                         if !indAux < (list.length acts)
                         then (found := true; resComp := !i; raise found)
                         else indAux := !indAux - (list.length acts))
                  !(!p.comps.(!i)).actSums
              with | found -> ignore 1)
           else ();
           if not !found
           then (resInd := !indAux - !(!p.comps.(!i)).nfnouts; incr i)
           else ()))
       done;
     ((!resComp), (!resInd)))
(***) (* Auxiliar function to find_name and find_action *)
let rec testAll l h =
  match l with
  | [] -> (true, [])
  | bname x :: tl ->
      let (res, resL) = testAll tl h
      in
        if res
        then
          if not (hashtbl.mem !h x)
          then (hashtbl.add !h x true; (true, (((freshName ()), x) :: resL)))
          else (true, resL)
        else (false, [])
  | fname x :: tl -> testAll tl h
  | iname i :: tl -> (false, [])
(***) (*** Finds an action given the subject name ***)
let findName p name actT nameC nameI =
  let s1 = !p.nComps in
  let found = ref false in
  let nameComp = ref nameC in
  let nameInd = ref nameI in
  let reveals = ref []
  in
    (while (!nameComp < s1) && (not !found) do
       (let (s2, acts) =
          if actT = inpType
          then
            ((!(!p.comps.(!nameComp)).nfninps),
             (ref !(!p.comps.(!nameComp)).fnInps))
          else
            ((!(!p.comps.(!nameComp)).nfnouts),
             (ref !(!p.comps.(!nameComp)).fnOuts))
        in
          (while (!nameInd < s2) && (not !found) do
             found := name = (getString !acts.(!nameInd).sub);
             if !found && (actT = outType)
             then
               (let (res, resL) =
                  testAll !acts.(!nameInd).obj
                    (ref (hashtbl.create (list.length !acts.(!nameInd).obj)))
                in if res then reveals := resL else found := false)
             else (); if not !found then incr nameInd else () done;
           if not !found
           then
             (let sumInd = ref (!nameInd - s2)
              in
                try
                  list.iter
                    (fun sum ->
                       match sum with
                       | (acts, taus, tests) ->
                           list.iter
                             (fun act ->
                                if !sumInd = 0
                                then
                                  (if
                                     (actT = act.t) &&
                                       ((isFnAct act) &&
                                          (name = (getString act.sub)))
                                   then
                                     if actT = outType
                                     then
                                       (let (res, resL) =
                                          testAll act.obj
                                            (ref
                                               (hashtbl.create
                                                  (list.length act.obj)))
                                        in
                                          if res
                                          then
                                            (found := true;
                                             reveals := resL;
                                             raise found)
                                          else ())
                                     else (found := true; raise found)
                                   else ();
                                   incr nameInd)
                                else decr sumInd)
                             acts)
                    !(!p.comps.(!nameComp)).actSums
                with | found -> ignore 1)
           else ();
           if not !found then (incr nameComp; nameInd := 0) else ()))
       done;
     ((!found), (!nameComp), (!nameInd), (!reveals)))
(***) (*** Finds an action given the action type ***)
let findAction p actT comp ind =
  let s1 = !p.nComps in
  let found = ref false in
  let actComp = ref comp in
  let actInd = ref ind in
  let reveals = ref []
  in
    (while (!actComp < s1) && (not !found) do
       (let (s2, acts) =
          if actT = inpType
          then
            ((!(!p.comps.(!actComp)).nfninps),
             (ref !(!p.comps.(!actComp)).fnInps))
          else
            ((!(!p.comps.(!actComp)).nfnouts),
             (ref !(!p.comps.(!actComp)).fnOuts))
        in
          (while (!actInd < s2) && (not !found) do
             if actT = outType
             then
               (let (res, resL) =
                  testAll !acts.(!actInd).obj
                    (ref (hashtbl.create (list.length !acts.(!actInd).obj)))
                in if res then (found := true; reveals := resL) else ())
             else found := true; if not !found then incr actInd else () done;
           if not !found
           then
             (let sumInd = ref (!actInd - s2)
              in
                try
                  list.iter
                    (fun sum ->
                       match sum with
                       | (acts, taus, tests) ->
                           list.iter
                             (fun act ->
                                if !sumInd = 0
                                then
                                  (if (actT = act.t) && (isFnAct act)
                                   then
                                     if actT = outType
                                     then
                                       (let (res, resL) =
                                          testAll act.obj
                                            (ref
                                               (hashtbl.create
                                                  (list.length act.obj)))
                                        in
                                          if res
                                          then
                                            (found := true;
                                             reveals := resL;
                                             raise found)
                                          else ())
                                     else (found := true; raise found)
                                   else ();
                                   incr actInd)
                                else decr sumInd)
                             acts)
                    !(!p.comps.(!actComp)).actSums
                with | found -> ignore 1)
           else ();
           if not !found then (incr actComp; actInd := 0) else ()))
       done;
     ((!found), (!actComp), (!actInd), (!reveals)))
(***) (* Auxiliar function to next_react_aux *)
let rec nameToString l =
  match l with | [] -> [] | hd :: tl -> (getString hd) :: (nameToString tl)
(***)
(*** Returns a process where a transition on a subject name or action type has taken place ***)
let nextReactAux p actT comp ind =
  if actT = inpType
  then
    (let act =
       if ind < !(!p.comps.(comp)).nfninps
       then !(!p.comps.(comp)).fnInps.(ind)
       else
         getActSums !(!p.comps.(comp)).actSums
           (ind - !(!p.comps.(comp)).nfninps) 0
     in
       reactLabel
         (actT, (getString act.sub), (freshNames (list.length act.obj) 0))
         comp ind p)
  else
    (let act =
       if ind < !(!p.comps.(comp)).nfnouts
       then !(!p.comps.(comp)).fnOuts.(ind)
       else
         getActSums !(!p.comps.(comp)).actSums
           (ind - !(!p.comps.(comp)).nfnouts) 0
     in
       reactLabel (actT, (getString act.sub), (nameToString act.obj)) comp
         ind p)
(***) (***) (* Auxiliar function to congruent_n *)
let rec cutSupp l supp fix =
  match l with
  | [] -> []
  | hd :: tl ->
      if (list.mem hd supp) || (list.mem hd fix)
      then cutSupp tl supp fix
      else hd :: (cutSupp tl supp fix)
(***) (* Auxiliar functions to is_cong_n_comp *)
let testNums c1 c2 =
  (!c1.nrests = !c2.nrests) &&
    ((!c1.nfnouts = !c2.nfnouts) &&
       ((!c1.nbnouts = !c2.nbnouts) &&
          ((!c1.nfninps = !c2.nfninps) &&
             ((!c1.nbninps = !c2.nbninps) &&
                ((!c1.ntests = !c2.ntests) && (!c1.nsums = !c2.nsums))))))
(* Handle name relations *)
let rec cutElem el l =
  match l with
  | [] -> []
  | hd :: tl -> if el = hd then tl else hd :: (cutElem el tl)
let rec related n1 n2 l =
  match l with
  | [] -> ([], (n1 = n2))
  | (ns1, ns2) :: tl ->
      if list.mem n1 ns1
      then
        if list.mem n2 ns2
        then
          if (list.length ns1) = 1
          then ((([ n1 ], [ n2 ]) :: tl), true)
          else
            ((([ n1 ], [ n2 ]) :: ((cutElem n1 ns1), (cutElem n2 ns2)) :: tl),
             true)
        else (((ns1, ns2) :: tl), false)
      else
        if list.mem n2 ns2
        then (((ns1, ns2) :: tl), false)
        else
          (let (resL, res) = related n1 n2 tl in (((ns1, ns2) :: resL), res))
let relateName n1 n2 fnames rests =
  match n1 with
  | fname fn1 ->
      (match n2 with
       | fname fn2 ->
           let (resL, res) = related fn1 fn2 !fnames in (fnames := resL; res)
       | _ -> false)
  | bname bn1 ->
      (match n2 with
       | bname bn2 ->
           let (resL, res) = related bn1 bn2 !rests in (rests := resL; res)
       | _ -> false)
  | iname i -> (match n2 with | iname k -> i = k | _ -> false)
let rec relateNameL l1 l2 fnames rests =
  match l1 with
  | [] -> true
  | hd :: tl ->
      (relateName hd (list.hd l2) fnames rests) &&
        (relateNameL tl (list.tl l2) fnames rests)
(* Handle sum comparison *)
let rec sums2ks acts taus tests =
  match acts with
  | [] ->
      (match taus with
       | [] ->
           (match tests with
            | [] -> []
            | hd :: tail -> (testK hd) :: (sums2ks acts taus tail))
       | hd :: tail -> (tauK hd) :: (sums2ks acts tail tests))
  | hd :: tail -> (actK hd) :: (sums2ks tail taus tests)
let selectSumJ nacts ntaus ntests i =
  if i < nacts
  then 0
  else if (i - nacts) < ntaus then nacts else nacts + ntaus
let topSumJ nacts ntaus ntests i =
  if i < nacts
  then nacts
  else
    if (i - nacts) < ntaus then nacts + ntaus else (nacts + ntaus) + ntests
(* Determines if two actions are equivalent *)
let rec isCongNAct k1 k2 fnames rests =
  match k1 with
  | actK act1 ->
      (match k2 with
       | actK act2 ->
           if
             ((list.length act1.obj) = (list.length act2.obj)) &&
               ((act1.cont = act2.cont) &&
                  ((act1.t = act2.t) &&
                     ((list.length act1.args) = (list.length act2.args))))
           then
             (relateName act1.sub act2.sub fnames rests) &&
               ((relateNameL act1.obj act2.obj fnames rests) &&
                  (relateNameL act1.args act2.args fnames rests))
           else false
       | _ -> false)
  | tauK tau1 ->
      (match k2 with
       | tauK tau2 ->
           if
             (tau1.tauCont = tau2.tauCont) &&
               ((list.length tau1.tauArgs) = (list.length tau2.tauArgs))
           then relateNameL tau1.tauArgs tau2.tauArgs fnames rests
           else false
       | _ -> false)
  | testK test1 ->
      (match k2 with
       | testK test2 ->
           if
             (test1.tcont = test2.tcont) &&
               ((list.length test1.targs) = (list.length test2.targs))
           then
             ((test1.idl = test2.idl) = (test1.idr = test2.idr)) &&
               (relateNameL test1.targs test2.targs fnames rests)
           else false
       | _ -> false)
  | sumK sum1 ->
      (match k2 with
       | sumK sum2 ->
           (match sum1 with
            | (acts1, taus1, tests1) ->
                (match sum2 with
                 | (acts2, taus2, tests2) ->
                     let size1acts = list.length acts1 in
                     let size2acts = list.length acts2
                     in
                       if size1acts <> size2acts
                       then false
                       else
                         (let sumVec1 = array.ofList acts1 in
                          let sumVec2 = array.ofList acts2 in
                          let corresp = array.create size1acts (-1) in
                          let marker = array.create size2acts false in
                          let i = ref 0 in
                          let j = ref 0 in
                          let backupFnames =
                            array.create (size1acts + 1) [] in
                          let backupRests = array.create (size2acts + 1) []
                          in
                            (backupFnames.(0) <- !fnames;
                             backupRests.(0) <- !rests;
                             let res = ref true
                             in
                               ((try
                                   while !i < size1acts do
                                     if
                                       (not marker.(!j)) &&
                                         (isCongNAct (actK sumVec1.(!i))
                                            (actK sumVec2.(!j)) fnames rests)
                                     then
                                       (corresp.(!i) <- !j;
                                        marker.(!j) <- true;
                                        incr i;
                                        backupFnames.(!i) <- !fnames;
                                        backupRests.(!i) <- !rests;
                                        j := 0)
                                     else
                                       if !j < (size1acts - 1)
                                       then incr j
                                       else
                                         (while
                                            (!j = (size1acts - 1)) &&
                                              (!i > 0)
                                            do fnames := backupFnames.(!i);
                                            rests := backupRests.(!i);
                                            decr i; j := corresp.(!i);
                                            marker.(!j) <- false;
                                            corresp.(!i) <- (-1) done;
                                          if !i = 0
                                          then raise False
                                          else incr j)
                                     done
                                 with
                                 | False ->
                                     (fnames := backupFnames.(0);
                                      res := false));
                                !res)))))
       | _ -> false)
(* Auxiliar functions to is_cong_n_comp *)
let selectAct c i =
  if i < !c.nfnouts
  then actK !c.fnOuts.(i)
  else
    (let j = i - !c.nfnouts
     in
       if j < !c.nbnouts
       then actK !c.bnOuts.(j)
       else
         (let k = j - !c.nbnouts
          in
            if k < !c.nfninps
            then actK !c.fnInps.(k)
            else
              (let m = k - !c.nfninps
               in
                 if m < !c.nbninps
                 then actK !c.bnInps.(m)
                 else
                   (let n = m - !c.nbninps
                    in
                      if n < !c.ntests
                      then testK !c.idTests.(n)
                      else
                        (let l = n - !c.ntests
                         in
                           if l < !c.ntaus
                           then tauK !c.actTaus.(l)
                           else sumK (list.nth !c.actSums (l - !c.ntaus)))))))
let selectJ c i =
  if i < !c.nfnouts
  then 0
  else
    (let k = i - !c.nfnouts
     in
       if k < !c.nbnouts
       then !c.nfnouts
       else
         (let n = k - !c.nbnouts
          in
            if n < !c.nfninps
            then !c.nfnouts + !c.nbnouts
            else
              (let m = n - !c.nfninps
               in
                 if m < !c.nbninps
                 then (!c.nfnouts + !c.nbnouts) + !c.nfninps
                 else
                   (let l = m - !c.nbninps
                    in
                      if l < !c.ntests
                      then
                        ((!c.nfnouts + !c.nbnouts) + !c.nfninps) + !c.nbninps
                      else
                        (let o = l - !c.ntests
                         in
                           if o < !c.ntaus
                           then
                             (((!c.nfnouts + !c.nbnouts) + !c.nfninps) +
                                !c.nbninps)
                               + !c.ntests
                           else
                             ((((!c.nfnouts + !c.nbnouts) + !c.nfninps) +
                                 !c.nbninps)
                                + !c.ntests)
                               + !c.ntaus)))))
let topJ c i =
  if i < !c.nfnouts
  then !c.nfnouts
  else
    (let k = i - !c.nfnouts
     in
       if k < !c.nbnouts
       then !c.nfnouts + !c.nbnouts
       else
         (let n = k - !c.nbnouts
          in
            if n < !c.nfninps
            then (!c.nfnouts + !c.nbnouts) + !c.nfninps
            else
              (let m = n - !c.nfninps
               in
                 if m < !c.nbninps
                 then ((!c.nfnouts + !c.nbnouts) + !c.nfninps) + !c.nbninps
                 else
                   (let l = m - !c.nbninps
                    in
                      if l < !c.ntests
                      then
                        (((!c.nfnouts + !c.nbnouts) + !c.nfninps) +
                           !c.nbninps)
                          + !c.ntests
                      else
                        (let o = l - !c.ntests
                         in
                           if o < !c.ntaus
                           then
                             ((((!c.nfnouts + !c.nbnouts) + !c.nfninps) +
                                 !c.nbninps)
                                + !c.ntests)
                               + !c.ntaus
                           else
                             (((((!c.nfnouts + !c.nbnouts) + !c.nfninps) +
                                  !c.nbninps)
                                 + !c.ntests)
                                + !c.ntaus)
                               + !c.nsums)))))
(* Determines if two components are equivalent *)
let isCongNComp p1 p2 i j fnames =
  let c1 = !p1.comps.(i) in
  let c1NumActs = countActsComp p1 i in
  let c2 = !p2.comps.(j) in
  let c2NumActs = countActsComp p2 j
  in
    if not (testNums c1 c2)
    then false
    else
      (let rests =
         ref
           [ ((getStringL (array.toList !c1.rests)),
              (getStringL (array.toList !c2.rests))) ] in
       let corresp = array.create c1NumActs (-1) in
       let marker = array.create c2NumActs false in
       let i = ref 0 in
       let j = ref (selectJ c2 !i) in
       let backupFnames = array.create (c1NumActs + 1) [] in
       let backupRests = array.create (c1NumActs + 1) []
       in
         (backupFnames.(0) <- !fnames;
          backupRests.(0) <- !rests;
          let res = ref true
          in
            ((try
                while !i < c1NumActs do
                  if
                    (not marker.(!j)) &&
                      (isCongNAct (selectAct c1 !i) (selectAct c2 !j) fnames
                         rests)
                  then
                    (corresp.(!i) <- !j;
                     marker.(!j) <- true;
                     incr i;
                     backupFnames.(!i) <- !fnames;
                     backupRests.(!i) <- !rests;
                     j := selectJ c2 !i)
                  else
                    if !j < ((topJ c2 !i) - 1)
                    then incr j
                    else
                      (while (!j = ((topJ c2 !i) - 1)) && (!i > 0) do
                         fnames := backupFnames.(!i);
                         rests := backupRests.(!i); decr i;
                         j := corresp.(!i); marker.(!j) <- false;
                         corresp.(!i) <- (-1) done;
                       if !i = 0 then raise False else incr j)
                  done
              with | False -> (fnames := backupFnames.(0); res := false));
             !res)))
(* Auxiliar functions and variables to congruent_n *)
let rec singles l l2 =
  match l with | [] -> l2 | hd :: tl -> ([ hd ], [ hd ]) :: (singles tl l2)
let globSupp = ref [] let globFix = ref []
let rec matchArgs args1 args2 el =
  match args1 with
  | [] -> [ el ]
  | hd :: tl ->
      ([ hd ], [ list.hd args2 ]) :: (matchArgs tl (list.tl args2) el)
(* Determines if two processes are equivalent *)
let congruentN val1 val2 =
  match val1 with
  | (p1, p1Args) ->
      (match val2 with
       | (p2, p2Args) ->
           let supp = !globSupp
           in
             if (numComps p1) <> (numComps p2)
             then false
             else
               (let fnP1 = freeNames p1 in
                let fnP2 = freeNames p2 in
                let fnP1Supp = cutSupp fnP1 supp p1Args in
                let fnP2Supp = cutSupp fnP2 supp p2Args
                in
                  if
                    ((list.length fnP1) <> (list.length fnP2)) ||
                      ((list.length fnP1Supp) <> (list.length fnP2Supp))
                  then false
                  else
                    (let fixed =
                       matchArgs p1Args p2Args (fnP1Supp, fnP2Supp) in
                     let fnames = ref (singles supp fixed) in
                     let corresp = array.create (numComps p1) (-1) in
                     let marker = array.create (numComps p2) false in
                     let backupFnames = array.create (1 + (numComps p1)) []
                     in
                       (backupFnames.(0) <- !fnames;
                        let i = ref 0 in
                        let j = ref 0 in
                        let res = ref true
                        in
                          ((try
                              while !i < (numComps p1) do
                                if
                                  (not marker.(!j)) &&
                                    (isCongNComp p1 p2 !i !j fnames)
                                then
                                  (corresp.(!i) <- !j;
                                   marker.(!j) <- true;
                                   incr i;
                                   backupFnames.(!i) <- !fnames;
                                   j := 0)
                                else
                                  if !j < ((numComps p2) - 1)
                                  then incr j
                                  else
                                    (while
                                       (!j = ((numComps p2) - 1)) && (!i > 0)
                                       do fnames := backupFnames.(!i);
                                       decr i; j := corresp.(!i);
                                       marker.(!j) <- false;
                                       corresp.(!i) <- (-1) done;
                                     if !i = 0 then raise False else incr j)
                                done
                            with | False -> res := false);
                           !res)))))
(***) (* Hash function for processes *)
let hashComps p =
  let res = ref 0 in
  let hashVal = hashtbl.hash "$x"
  in
    (for i = 0 to (numComps p) - 1 do
       (let comp = !p.comps.(i)
        in
          (for j = 0 to !comp.nfnouts - 1 do
             res := (!res + !comp.fnOuts.(j).cont) + hashVal
           done;
           for j = 0 to !comp.nfninps - 1 do
             res := (!res + !comp.fnInps.(j).cont) + hashVal
           done;
           for j = 0 to !comp.nbnouts - 1 do
             res := (!res + !comp.bnOuts.(j).cont) + hashVal
           done;
           for j = 0 to !comp.nbninps - 1 do
             res := (!res + !comp.bnInps.(j).cont) + hashVal
           done;
           for j = 0 to !comp.ntests - 1 do
             res := (!res + !comp.idTests.(j).tcont) + hashVal
           done;
           for j = 0 to !comp.ntaus - 1 do
             res := (!res + !comp.actTaus.(j).tauCont) + hashVal
           done;
           list.iter
             (fun sum ->
                match sum with
                | (acts, taus, tests) ->
                    (list.iter
                       (fun act -> res := (!res + act.cont) + hashVal) acts;
                     list.iter
                       (fun tau -> res := (!res + tau.tauCont) + hashVal)
                       taus;
                     list.iter
                       (fun test -> res := (!res + test.tcont) + hashVal)
                       tests))
             !comp.actSums))
     done;
     !res)
(***) (*** Top level process set type ***)
module ProcessHash =
  struct
    
    type T = (Process, (List[String]))
    
    let equal p1 p2 = congruentN p1 p2
    let hash p =
      match p with | (proc, l) -> (numComps proc) * (hashComps proc)
  end
module ProcessSet = hashtbl.make(processHash)

type ProcessSet = ProcessSet.T[Bool]
 (*** Creates a process set ***)
let createPset p l =
  let pset = processSet.create 100 in (processSet.add pset (p, l) true; pset)
(***) (*** Adds a process to a process set ***)
let addToPset pset p l = processSet.add pset (p, l) true (***)
(*** Removes a process from a process set ***)
let removeFromPset pset p l = processSet.remove pset (p, l) (***)
(*** Determines if there exists an equivalent process in the process set ***)
let rec existsCongruentN p l pset supp =
  (globSupp := supp;
   let res = processSet.mem pset (p, l) in (globSupp := []; res))
