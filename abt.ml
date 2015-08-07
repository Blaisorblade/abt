(* -*- mode: ocaml; -*- *)
(* A copy of https://gist.github.com/neel-krishnaswami/834b892327271e348f79 *)

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

type 'a monoid = {unit : 'a ; join : 'a -> 'a -> 'a}

type var = string
module V = Set.Make(struct type t = var let compare = compare end)

module type ABT = sig
  type 'a signature
  type 'a f = Var of var | Abs of var * 'a | Tm of 'a signature
  val map : ('a -> 'b) -> 'a f -> 'b f

  type t
  val into : t f -> t
  val out  : t -> t f

  val freevars : t -> V.t
  val var : V.elt -> t
  val abs : V.elt * t -> t
  val tm : t signature -> t

  val subst : t -> var -> t -> t
end

module type SIGNATURE = sig
  include FUNCTOR

  val join : 'a monoid -> 'a t -> 'a
end

module Abt(F : SIGNATURE) : ABT with type 'a signature := 'a F.t =
struct
  type 'a f =
    | Var of var
    | Abs of var * 'a
    | Tm of 'a F.t

  let map f = function
    | Var x -> Var x
    | Abs(x, e) -> Abs(x, f e)
    | Tm t -> Tm (F.map f t)

  type t = In of V.t * t f

  let freevars (In(vs, _)) = vs
  let out (In(_, t)) = t

  let m = {unit = V.empty; join = V.union}

  let var x = In(V.singleton x, Var x)
  let abs(z, e) = In(V.remove z (freevars e), Abs(z, e))
  let tm t = In(F.join m (F.map freevars t), Tm t)

  let into = function
    | Var x -> var x
    | Abs(x, e) -> abs(x, e)
    | Tm t -> tm t

  let rec fresh vs v =
    if V.mem v vs then fresh vs (v ^ "'") else v

  let rec rename x y (In(fvs, t)) =
    match t with
    | Var z -> if x = z then var y else var z
    | Abs(z, e) -> if x = z then abs(z, e) else abs(z, rename x y e)
    | Tm v -> tm (F.map (rename x y) v)

  let rec subst t x body =
    match out body with
    | Var z when x = z -> t
    | Var z            -> var z
    | Abs(x, e) ->
      let x' = fresh (V.union (freevars t) (freevars body)) x in
      let e' = subst t x (rename x x' e) in
      abs(x', e')
    | Tm body -> tm (F.map (subst t x) body)
end

module Lambda =
struct
  type tp = Base | Arrow of tp * tp
  type 'a t = Lam of 'a | App of 'a * 'a | Let of 'a * 'a | Annot of tp * 'a
  let map f = function
    | Lam x      -> Lam (f x)
    | App (x, y) -> App(f x, f y)
    | Let (x, y) -> Let(f x, f y)
    | Annot(t, x) -> Annot(t, f x)

  let join m = function
    | Lam x -> x
    | App(x, y) -> m.join x y
    | Let(x, y) -> m.join x y
    | Annot(_, x) -> x
end

module Syntax = Abt(Lambda)

module Bidir = struct
  open Lambda
  open Syntax
  type ctx = (var * tp) list

  let is_synth = function
    | Tm (Lam _) | Tm (Let (_, _)) -> false
    | _ -> true
  let is_check e = not(is_synth e)

  let unabs e =
    match out e with
    | Abs(x, e) -> (x, e)
    | _ -> assert false

  let rec check ctx e tp =
    match out e, tp with
    | Tm (Lam t), Arrow(tp1, tp') ->
      let (x, e') = unabs t in
      check ((x, tp1) :: ctx) e' tp'
    | Tm (Lam _), _               -> failwith "expected arrow type"
    | Tm (Let(e', t)), _ ->
      let (x, e'') = unabs t in
      let tp1 = synth ctx e' in
      check ((x, tp1) :: ctx) e'' tp
    | body, _ when is_synth body ->
      if tp = synth ctx e then () else failwith "Type mismatch"
    | _ -> assert false

  and synth ctx e =
    match out e with
    | Var x -> (try List.assoc x ctx with Not_found -> failwith "unbound variable")
    | Tm(Annot(tp, e)) -> let () = check ctx e tp in tp
    | Tm(App(f, e))  ->
      (match synth ctx f with
       | Arrow(tp, tp') -> let () = check ctx e tp in tp'
       | _ -> failwith "Applying a non-function!")
    | body when is_check body -> failwith "Cannot synthesize type for checking term"
    | _ -> assert false
end
