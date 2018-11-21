(** This needs MetaOCaml -- see the exercise sheet for installation instructions: *)

(* #use "ex3.ml";; *)

(** The EXP signature.  This gives the type of the various interpreter
   modules.

   There is an abstract type 't' for terms, and signatures for
   functions that create values of type 't'.

   While 't' is abstract here, each module will define 't' in a
   different way to implement a different type of interpretation:
   evaluation, compilation, partial evaluation, cps transformation,
   etc. *)
module type EXP = sig
  type 'a t
  val int : int -> int t
  val add : int t -> int t -> int t 
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val lam : ('a t -> 'b t) -> ('a -> 'b) t
  val app : ('a -> 'b) t -> 'a t -> 'b t
  val print : int t -> unit t
end

(** Now that we have the interface, we can write some example terms *)
module Example (E: EXP) =
struct
  open E
  (** The double application function Î»f.Î»x.f (f x) *)
  let t0 : ((int -> int) -> int -> int) E.t = 
      (lam (fun f ->
           lam (fun x ->
               app f (app f x))))

  (** An application of t0 to the identity *)
  let t1 = app t0 (lam (fun y -> y))

  (** A function that doesn't specialize (partially evaluate) very well:
        Î»b.(2 + if b then 3 else 4)
   *)
  let t2 = 
    lam (fun b ->
        add (int 2)
          (if_ b (int 3) (int 4)))

  (** A term that behaves differently under CBN and CBV
      (Î»b.Î»e.if b then e else print 1) false (print 2)

      CBV: print 2; print 1
      CBN: print 1
 *)
  let t3 =
    app
      (app
         (lam (fun b ->
              (lam (fun e ->
                   if_ b
                     e
                     (print (int 1))))))
         (bool false))
      (print (int 2))
end

(** A very simple interpretation of EXP: almost the identity,
    but we use thunks so that we can control the evaluation of
    'if' rather than always evaluating both branches *)
module Eval : EXP with type 'a t = unit -> 'a =
struct
  type 'a t = unit -> 'a
  let int x = fun () -> x
  let bool b = fun () -> b
  let add x y = fun () -> x () + y ()
  let lam f = fun () -> (fun x -> f (fun () -> x) ())
  let app f x () = f () (x ())
  let if_ b t e () = if b () then t () else e ()
  let print i () = print_int (i ())
end
(** Evaluate the examples using Eval *)
module Eval_examples = Example(Eval)
let t0_eval = Eval_examples.t0
let t1_eval = Eval_examples.t1
let t2_eval = Eval_examples.t2
let t3_eval = Eval_examples.t3

(** There follow some auxiliary definitions for a tracing
   interpretation.  Rather than executing the effects (i.e. the uses
   of 'print') in the interpreter, as Eval does, evalauting a term
   using the tracing interpretation will simply collect a trace of the
   effects that would be executed *)

(** A type of terms for the tracing interpreter.
    Each constructor pairs a value with a list of effects
    that the program executed during the construction of the value. *)
type effects = int list
type 'a trace =
  Fun : ('a trace -> 'b trace) * effects -> ('a -> 'b) trace
| Int : int * effects -> int trace
| Bool : bool * effects -> bool trace
| Unit : unit * effects -> unit trace

(** 'effects' retrieves the list of effects that were executed 
    during the construction of a value *)
let effects : type a. a trace -> effects = function
  | Fun (_, es) -> es
  | Int (_, es) -> es
  | Bool (_, es) -> es
  | Unit (_, es) -> es

(** 'cleartrace' removes the list of effects that were executed 
    during the construction of a value, leaving just the value *)
let cleartrace : type a. a trace -> a trace = function
  | Fun (v, _) -> Fun (v, [])
  | Int (v, _) -> Int (v, [])
  | Bool (v, _) -> Bool (v, [])
  | Unit (v, _) -> Unit (v, [])

(** '>>' composes two programs, catenating the lists of their effects.
    The result of 'a >> b' is the result of 'b', and the effects
    are the effects of 'a' followed by the effects of 'b'.
    That is, 'a >> b' behaves like 'a; b' in an imperative language. *)
let (>>) : type a b. a trace -> b trace -> b trace =
  fun x y ->
  match y with
  | Fun (v, es) -> Fun (v, effects x @ es)
  | Int (v, es) -> Int (v, effects x @ es)
  | Bool (v, es) -> Bool (v, effects x @ es)
  | Unit (v, es) -> Unit (v, effects x @ es)

(** A CBV tracing implementation of EXP. *)
module Trace : EXP with type 'a t = 'a trace =
struct
  type 'a t = 'a trace
  let int x = Int (x, [])
  let bool x = Bool (x, [])
  let add (Int (x,es)) (Int (y,es')) = Int (x + y, es @ es')
  (** (a): complete the remaining functions, replacing
      'assert false' with your implementation.

      Note: use CBV application order
 *)
  let lam f = Fun(f, [])(* ('a t -> 'b t) -> ('a -> 'b) t *)
  let app (Fun(f, es)) = 
    fun x -> 
      ((Fun(f, es)) >> x) >> f(cleartrace(x))
  let if_ (Bool (b, es)) t e = 
    Bool(b, es) >> (if (b) then t else e)
  let print (Int (i, es)) = Unit ((), i :: es)
end

module TraceCBN : EXP with type 'a t = 'a trace =
struct
  type 'a t = 'a trace
  let int x = Int (x, [])
  let bool x = Bool (x, [])
  let add (Int (x,es)) (Int (y,es')) = Int (x + y, es @ es')
  (** (a): complete the remaining functions, replacing
      'assert false' with your implementation

      Note: use CBN application order *)
  let lam f =  Fun(f, [])
  let app (Fun(f, es)) = fun x -> 
    Fun(f, es) >> f(x)  
  let if_ (Bool(b, es)) t e  = Bool(b, es) >> (if (b) then t else e)
  let print (Int (i, es)) = Unit ((), i :: es)
end

(** Naive compilation using MetaOCaml's brackets and escapes  *)
module Compile : EXP with type 'a t = 'a code =
struct
  type 'a t = 'a code
  let int (x:int) = .<x>.
  let bool (x:bool) = .<x>.
  let add x y = .< .~x + .~y >.
  let lam f = .< fun x -> .~(f .<x>.) >.
  let app f x = .< .~f .~x >.
  let if_ b t e = .< if .~b then .~t else .~e >.
  let print e = .< print_int .~e >.
end
(** Compile the examples using Compile *)
module Compile_examples = Example(Compile)
let t0_compile = Compile_examples.t0
let t1_compile = Compile_examples.t1
let t2_compile = Compile_examples.t2
let t3_compile = Compile_examples.t3

(** A simple partial evaluator, similar to the one shown in lectures *)
type _ static1 =
  | Int : int -> int static1
  | Bool : bool -> bool static1
  | Unknown : _ static1
type 'a sd1 = {
    sta: 'a static1;
    dyn: 'a code;
  }
let rec resid1 : type a. a sd1 -> a code = function
  | { sta = Unknown; dyn } -> dyn
  | { sta = Int x } -> .< x >.
  | { sta = Bool x } -> .< x >.

module PE_basic : EXP with type 'a t = 'a sd1 =
struct
  (** (c) complete the implementation of PE *)
  type 'a t = 'a sd1
  let int (i : int) = { sta = Int i; dyn = .<i>. }
  let bool (b : bool) = { sta = Bool b; dyn = .<b>. }
  let add x y = match x.sta, y.sta with
    | Int 0, Int _ -> y
    | Int _, Int 0 -> x
    | Int l, Int r -> let s = l + r in
                      { sta = Int s; dyn = .< s >. }
    | _ -> { sta = Unknown; dyn = .< .~(x.dyn) + .~(y.dyn) >. }
  let lam f   = let dyn = .< fun x -> .~(resid1 (f {sta = Unknown; dyn = .<x>.})) >.
                in { sta = Unknown; dyn = dyn }
  let app f x = { sta = Unknown; dyn = .< .~(resid1 f) .~(resid1 x) >. }
  let print e = { sta = Unknown; dyn = .< print_int .~(resid1 e) >. }
  let if_ b t e =
    match b.sta with
      Bool true -> t
    | Bool false -> e
    | Unknown -> 
        let dyn = .< if .~(b.dyn) then .~(resid1 t) else .~(resid1 e) >. 
        in
         { sta = Unknown; dyn = dyn }
end
				 

(** Next we'll build a higher-order partial evaluator.  The type 'sd'
   is a representation of partially-static data, where each value is a
   pair of an optional static (known) value and a dynamic (unknown)
   value. *)
type _ static =
  | Int : int -> int static
  | Bool : bool -> bool static
  (* Monad?? *)
  | Fun  : ('a sd -> 'b static) -> ('a -> 'b) static
  | Unknown : _ static
and 'a sd = {
    sta: 'a static;
    dyn: 'a code;
  }
(** The 'resid' function converts partially-static values into
   fully dynamic values â€” i.e. into code.

   We favour using the static portions over the dynamic portions
   wherever possible, for the best (i.e. most-evaluated) possible
   results *)
let rec resid : type a. a sd -> a code = function
  | { sta = Unknown; dyn } -> dyn
  | { sta = Int x } -> .< x >.
  | { sta = Bool x } -> .< x >.
  | { sta = Fun f; dyn = fd } -> fd

(** Here's a starting point for the higher-order partial evaluator â€”
   an interpretation of EXP where the term type is instantiated to sd
   *)
module PE : EXP with type 'a t = 'a sd =
struct
  (** (c) complete the implementation of PE *)
  type 'a t = 'a sd
  let int (i : int) = { sta = Int i; dyn = .<i>. }
  let bool (b : bool) = { sta = Bool b; dyn = .<b>. }
  let add x y = match x.sta, y.sta with
    | Int 0, Int _ -> y
    | Int _, Int 0 -> x
    | Int l, Int r -> let s = l + r in
                      { sta = Int s; dyn = .< s >. }
    | _ -> { sta = Unknown; dyn = .< .~(x.dyn) + .~(y.dyn) >. }
  let lam (f: 'a t -> 'b t): ('a -> 'b) t  =
    {
      sta = Fun(fun a -> (f a).sta);
      dyn = .< fun at -> .~(resid(f({sta = Unknown; dyn = .<at>.})))>.
    } 
  let app f   = fun x -> 
    let static = match f.sta with
      | Fun f1 -> f1 x
      | Unknown -> Unknown
    in
      {sta= static; dyn = .< .~(f.dyn) .~(x.dyn)>.}
  let print (e: int t) : unit t = match e.sta  with 
    (* I don't think we want to eagerly evaluate print at compile time, but can eval the e*)
    | Int x -> 
      {sta = Unknown; dyn = .< (print_int x) >.}
    | _ -> 
     { sta = Unknown; dyn = .< (print_int .~(resid e)) >. }
  let if_ b t e =
    match b.sta with
      Bool true -> t
    | Bool false -> e
    | Unknown -> {
       sta = Unknown;
       dyn = .< if .~(b.dyn) then .~(resid(t)) else .~(resid(e)) >.
      }
end
(** Partially-evaluate the example terms.  *)
(*
module PE_examples = Example(PE) ;;
let t0_pe =  resid PE_examples.t0 ;;
let t1_pe =  resid PE_examples.t1 ;;
let t2_pe =  resid PE_examples.t2 ;;
let t3_pe =  resid PE_examples.t3 ;;*)

(** The 'equalp' function checks whether two 'static' values
    are equal.  If it is not possible to determine
    equality then 'equalp' returns 'Unknown'. *)
type equal = Yes | No | Unknown
let rec equalp : type a. a static -> a static -> equal =
  fun l r ->
    match (l, r) with
    | (Int il, Int ir) -> if il = ir then Yes else No
    | (Bool bl, Bool br) -> if bl = br then Yes else No
    | _ -> Unknown (* Not going to get into function equality. *)

module PE2 : EXP with type 'a t = 'a sd =
struct
  (* (d) complete the implementation of PE2.
     Your implementation should make appropriate use of 'equalp',
     but it should be otherwise equivalent to PE *)
  type 'a t = 'a sd
  let int (i : int) = { sta = Int i; dyn = .<i>. }
  let bool (b : bool) = { sta = Bool b; dyn = .<b>. }
  let add x y = match x.sta, y.sta with
    | Int 0, Int _ -> y
    | Int _, Int 0 -> x
    | Int l, Int r -> let s = l + r in
                      { sta = Int s; dyn = .< s >. }
    | _ -> { sta = Unknown; dyn = .< .~(x.dyn) + .~(y.dyn) >. }
  let lam (f: 'a t -> 'b t): ('a -> 'b) t  =
    {
      sta = Fun(fun a -> (f a).sta);
      dyn = .< fun at -> .~(resid(f({sta = Unknown; dyn = .<at>.})))>.
    } 
  let app f   = fun x -> 
    let static = match f.sta with
      | Fun f1 -> f1 x
      | Unknown -> Unknown
    in
      {sta= static; dyn = .< .~(f.dyn) .~(x.dyn)>.}
  let print (e: int t) : unit t = match e.sta  with 
    (* I don't think we want to eagerly evaluate print at compile time, but can eval the e*)
    | Int x -> 
      {sta = Unknown; dyn = .< (print_int x) >.}
    | _ -> 
     { sta = Unknown; dyn = .< (print_int .~(resid e)) >. }
  let if_ b t e =
    match b.sta with
      Bool true -> t
    | Bool false -> e
    | Unknown -> match (equalp t.sta e.sta) with
      (* Issue - this optimises out prints *)
      | Yes -> t
      | _ -> { 
        sta = Unknown; 
        dyn = .< if .~(b.dyn) then .~(resid(t)) else .~(resid(e)) >.
      }

end
(* module PE2_examples = Example(PE2)
 * let t0_pe2 =  resid PE2_examples.t0
 * let t1_pe2 =  resid PE2_examples.t1
 * let t2_pe2 =  resid PE2_examples.t2
 * let t3_pe2 =  resid PE2_examples.t3 *)


(** A CPS interpretation of EXP, where each term
    is interpreted as a continuation-accepting function.

    Making 'cps' a record type makes it possible to use higher-order
    polymorphism --- i.e. to pass around polymorphic functions
    as arguments and return values.

    The type of cps is equivalent to   âˆ€b. ('a â†’ b) â†’ b    *)

(* I've renamed k to be runCont in this record type, to make it clearer when used with the continuation fns themselves*)
type 'a cps = {runCont: 'b. ('a -> 'b) -> 'b}
(* To Run: *)
(* CPS.(lam(fun f -> app f ( app f ( int 3) ) ) ).runCont(fun x -> x ) succ ;; *)

module CPS : EXP with type 'a t = 'a cps =
struct
  (** (e) complete the implementation of CPS. *)
  (* Helper functions (I.e monad/functor typeclass functions using scala nomenclature )*)
  (* 'a cps -> ('a -> 'b cps) -> 'b cps *)
  let flatmap : type a b. a cps -> (a -> b cps) -> b cps  = 
  fun ma f ->
   { runCont = fun k -> ma.runCont(fun a -> (f a).runCont(k))}
  (* 'a cps -> ('a -> 'b) -> 'b cps *)
  let map : type a b. a cps -> (a -> b) -> b cps = 
  fun ma f -> 
    {runCont = fun k -> ma.runCont(fun a -> k(f(a)))}
  (* 'a -> 'a cps *)
  let point : type a. a -> a cps = 
    fun a -> {runCont = fun k -> k(a)}

  type 'a t = 'a cps
  let int i = point(i)
  let bool b = point(b)
  let add x y = flatmap x (fun x1 -> (map y (fun y1 -> x1 + y1)))

    (* ('a t -> 'b t) -> ('a -> 'b) t  *)
  let lam : ('a t -> 'b t) -> ('a -> 'b) t =
    fun f ->  {runCont = fun k -> k(fun a -> (f (point a)).runCont(fun b -> b))}

   (*  ('a -> 'b) t -> 'a t -> 'b t *)
  let app f x = flatmap f (fun g -> (map x (fun x1 -> (g x1))))
    

  let if_ c t e =
   flatmap c (fun b -> if b then t else e)
  let print e = (map e print_int)

end
(** Interpret the example terms using CPS *)
(* module CPS_examples = Example(CPS) *)
(** Pass in the identity continuation to recover the values *)
(* let t0_cps =  CPS_examples.t0
 * let t1_cps =  CPS_examples.t1
 * let t2_cps =  CPS_examples.t2
 * let t3_cps =  CPS_examples.t3 *)

(** Another version of CPS, parameterized by a second interpretation
   of EXP.  Rather than ultimately evaluating in OCaml and passing the
   results to the continuation, we'll evaluate using the second
   interpretation, E. *)


module CPS2 (E: EXP) :
sig
  type 'a cps_ = {runCont: 'b. ('a E.t -> 'b E.t) -> 'b E.t}
  include EXP with type 'a t = 'a cps_
end =
struct
  
 

  type 'a cps_ = {runCont: 'b. ('a E.t -> 'b E.t) -> 'b E.t}

 (* Bastardised monad helper fns*)
  let flatmap : type a b. a cps_ -> (a E.t -> b cps_) -> b cps_ = 
  fun ma f ->
    {runCont = fun k -> ma.runCont(fun at -> (f at).runCont(k))}
  let point : type a. a E.t -> a cps_ = 
  fun at -> {runCont = fun k -> k at}

  let map : type a b. a cps_ -> (a E.t -> b E.t) -> b cps_ =
  fun ma f ->  
    {runCont = fun k -> ma.runCont(fun at -> k(f(at)))}

  type 'a t = 'a cps_
  let int i =
    point(E.int i)
  let bool b =
    point(E.bool b)
  let add x y =
    flatmap x (fun xt -> map y (fun yt -> E.add xt yt))
  let lam f =
     {runCont = fun k -> k(E.lam(fun (at: 'a E.t) -> (f(point at)).runCont(fun bt -> bt)))}
  let app f x =
    flatmap f (fun ft -> (map x (fun xt -> E.app ft xt)))
  let if_ c t e =
    flatmap c (fun bt -> (flatmap t (fun tt -> (map e (fun et -> (E.if_ bt tt et))))))
  let print e =
    map e (fun et -> E.print(et))
end

(** With the parameterized CPS interpretation we can combine
    CPS and PE2.  Are the results any better than PE2 alone? *)
(* module CPS_PE = CPS2(PE) *)
(** Let's interpret the terms using CPS_PE, and look at the results. *)
(* module CPS_PE_examples = Example(CPS_PE) *)
(* let t0_cps_pe =  CPS_PE_examples.t0
 * let t1_cps_pe =  CPS_PE_examples.t1
 * let t2_cps_pe =  CPS_PE_examples.t2
 * let t3_cps_pe =  CPS_PE_examples.t3 *)

(** Finally, a normalizing implementation with some constraints
    on the form of generated terms (for which see the exercise sheet) *)
type 'a normalized =
  | Return : 'a value -> 'a normalized
  | Let : 'b effect * ('b value -> 'a normalized) -> 'a normalized
  | If : bool value * 'a normalized * 'a normalized -> 'a normalized
and 'a value =
  | Var : 'a code -> 'a value
  | Int : int -> int value
  | Bool : bool -> bool value
  | Lam : ('a value -> 'b normalized) -> ('a -> 'b) value
and 'a effect  = 
  | Print : int normalized -> unit effect
  | Add: int normalized * int normalized -> int effect
  | App: ('a -> 'b) normalized * 'a normalized -> 'b effect
  | Fun: ('a -> 'b) value -> ('a -> 'b) effect

let rec residn : type a. a normalized -> a code = function
    Return v -> residnv v
  | Let (e, k) -> .< let x = .~(residne e) in .~(residn (k (Var .<x>.))) >.
  | If (c, t, e) -> .< if .~(residnv c) then .~(residn t) else .~(residn e) >.
and residnv : type a. a value -> a code = function
  | Var x -> x
  | Int i -> .<i>.
  | Bool b -> .<b>.
  | Lam f -> .< fun x -> .~(residn (f (Var .<x>.))) >.
and residne : type a. a effect -> a code = function
    | Print i -> .< print_int .~(residn i) >.
    | Add (a, b) -> .< .~(residn a) + .~(residn b)>.
    | App (f, x) -> .< (.~ (residn f) .~(residn x)) >.
    | Fun f -> .< .~(residnv f)>.


module Normal : EXP with type 'a t = 'a normalized =
struct
  type 'a t = 'a normalized

  (* Hint: write a function '>>=' with the given type
     and use it to combine values of type 't' in your
     implementations of the functions below. *)
  let rec (>>=) : type a b. a t -> (a value -> b t) -> b t =
    fun ma k -> match ma with 
      | Return(av) -> k(av)
      | Let(be, f) -> Let(be, fun bv -> (f bv) >>= k)
      | If(bv, t, e) -> If(bv, t >>= k, e >>= k)
        

  let int x = Return (Int x)
  let bool x = Return (Bool x)
  let app f = fun xn ->
    f >>= (fun fv -> Let(App(Return fv, xn), (fun res -> Return res)))
  let lam : ('a t -> 'b t) -> ('a -> 'b) t = fun f ->
    Let (Fun (Lam(fun v -> f(Return v))),  (fun res -> Return res))
  let if_ c t e =
    c >>= (fun b -> If(b, t, e))
  let add x y =
    x >>= (fun xv -> y >>= (fun yv -> Let(Add(Return(xv), Return(yv)), fun res -> Return res)))
  let print e =
    e >>= (fun  v -> 
    Let (Print (Return v), (fun u -> Return u)))
end

(* 
# residn Normal.(
  lam (fun x -> 
    app (lam(fun c -> c))(add(add(int 3) x) x) 
  ) 
) ;;
- : ( int -> int ) code =
. < fun x_31 -> 
  let x_32 = 3 + x_31 in
  let x_33 = x_32 + x_31 in
  x_33
>.


# residn Normal.(
  lam(fun b ->
    lam(fun x ->
      add (int 4)
        (if_ b
          (int 0)
          (
            app(lam(fun x -> x))
            (add(int 3) x) 
          ) 
        )
      )
    )
  ) ;;


- : ( bool -> int -> int ) code = 
  . <
    fun x_24 ->
      fun x_25 ->
        if x_24
        then let x_28 = 4 + 0 in x_28
        else (
          let x_26 = 3 + x_25 in
          let x_27 = 4 + x_26 in
          x_27
        )
      >.


residn Normal.(
  lam (fun x -> x)) 
);;
*)