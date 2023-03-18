module Var : sig
  type t [@@deriving compare, sexp]

  val of_string : string -> t
  val to_string : t -> string

  include Core.Comparable.S with type t := t

  module Map :
    Core.Map.S
      with type Key.t = t
       and type Key.comparator_witness = comparator_witness

  module Set :
    Core.Set.S
      with type Elt.t = t
       and type Elt.comparator_witness = comparator_witness

  val equal : t -> t -> bool
  val fresh : unit -> t
end = struct
  include Core.String

  let count = ref 0

  let fresh () =
    let t = !count in
    count := t + 1;
    "#x" ^ Int.to_string t
end

(* Top-level commands *)
type 'a cmd = Eval of 'a | Test of 'a * 'a | Print of 'a

(* Representation of the program *)
module type SYNTAX = sig
  type t
  type view = Var of Var.t | Lam of Var.t * t | App of t * t

  val into : view -> t
  val out : t -> view
  val subst : t * Var.t -> t -> t
  val aequiv : t -> t -> bool
  val to_string : t -> string
end

module Ast = struct
  type t = Var of Var.t | Lam of Var.t * t | App of t * t
  type view = t = Var of Var.t | Lam of Var.t * view | App of view * view

  let into t = t
  let out t = t

  (* [e'/x]e, that is, Substitute e' for x in e *)
  let rec subst (e', x) e =
    match e with
    | Var var -> if Var.equal var x then e' else Var var
    | Lam (var, t1) -> if Var.equal var x then e else Lam (var, subst (e', x) t1)
    | App (p, q) -> App (subst (e', x) p, subst (e', x) q)

  (* Test for alpha equivalence between e1 and e2 *)
  let rec aequiv e1 e2 =
    match (e1, e2) with
    | Var x, Var y -> Var.equal x y
    | App (x1, y1), App (x2, y2) -> aequiv x1 x2 && aequiv y1 y2
    | Lam (v1, t1), Lam (v2, t2) ->
        let new_var = Var (Var.fresh ()) in
        aequiv (subst (new_var, v1) t1) (subst (new_var, v2) t2)
    | _ -> false

  let rec to_string e =
    match e with
    | Var v -> Var.to_string v
    | Lam (x, e) -> Printf.sprintf "\\%s.%s" (Var.to_string x) (to_string e)
    | App (e1, e2) ->
        let e1_format =
          match e1 with
          | Lam _ -> format_of_string "(%s)"
          | _ -> format_of_string "%s"
        in
        let e2_format =
          match e2 with
          | Lam _ | App _ -> format_of_string "(%s)"
          | _ -> format_of_string "%s"
        in
        Printf.sprintf
          (e1_format ^^ " " ^^ e2_format)
          (to_string e1) (to_string e2)
end

module Abt = struct
  type t = BVAR of int | FVAR of Var.t | LAM of t | APP of t * t

  (* A 'view' of a lambda term t "exposes" its top level, that is
   * if it is a lambda, it exposes the binded term into two separate 
   * entity: a free variable and a term with the old binder replaced by the
   * the free variable. This exposes the top level binder as a free variable.
   *
   * Terms are moved into/out of views through functions into/out 
   *
   * e.g. (#n denotes "BVar n")
   * 
   *   LAM #0 ---OUT--> 
   *         <--INTO--- Lam (x, FVAR x) 
   *
   *   LAM (LAM (APP #0 #1)) ---OUT--> 
   *                         <--INTO-- Lam (x, (LAM (APP #0 (FVAR x))))
   * *)
  type view = Var of Var.t | Lam of Var.t * t | App of t * t

  let rec unbindHelper m l n =
    match m with
    | FVAR _ -> m
    | BVAR x -> if x == l then n else if x > l then BVAR (x - 1) else m
    | LAM x -> LAM (unbindHelper x (l + 1) n)
    | APP (x, y) -> APP (unbindHelper x l n, unbindHelper y l n)

  let unbind m n = unbindHelper m 0 n

  (* [e'/x] e, that is, Substitute e' for x in e*)
  let rec subst (e', x) e =
    match e with
    | FVAR x' -> if Var.equal x' x then e' else e
    | LAM x' -> LAM (subst (e', x) x')
    | APP (y, z) -> APP (subst (e', x) y, subst (e', x) z)
    | BVAR _ -> e

  (* Test for alpha equivalence between e1 and e2 *)
  let rec aequiv e1 e2 =
    match (e1, e2) with
    | FVAR x, FVAR y -> Var.equal x y
    | BVAR x, BVAR y -> x == y
    | LAM x, LAM y -> aequiv x y
    | APP (x1, y1), APP (x2, y2) -> aequiv x1 x2 && aequiv y1 y2
    | _ -> false

  (* The following are two helper function you probably would need *)

  let rec bindh l x y =
    match y with
    | FVAR p -> if Var.equal p x then BVAR l else y
    | BVAR _ -> y
    | LAM p -> LAM (bindh (l + 1) x p)
    | APP (p, q) -> APP (bindh l x p, bindh l x q)

  (* binds free variable given by Var.t *)
  let bind (x, m) = LAM (bindh 0 x m)

  (* let  *)

  let into n =
    match n with
    | Var x -> FVAR x
    | Lam (x, y) -> bind (x, y)
    | App (x, y) -> APP (x, y)

  let rec outh l n =
    match l with
    | FVAR x -> Var x
    | BVAR _ -> failwith "cannot find such binding"
    | APP (x, y) -> App (into (outh x n), into (outh y n))
    | LAM x ->
        let name = Var.fresh () in
        Lam (name, into (outh (unbind x (FVAR name)) (n + 1)))

  (* unbinds the the top most bounded variable by freeing it as Var.t
     let unbind : t -> Var.t * t = failwith "Unimplemented" *)
  let out l = outh l 0

  let rec to_string e =
    match out e with
    | Var v -> Var.to_string v
    | Lam (x, e) -> Printf.sprintf "\\%s.%s" (Var.to_string x) (to_string e)
    | App (e1, e2) ->
        let e1_format =
          match out e1 with
          | Lam _ -> format_of_string "(%s)"
          | _ -> format_of_string "%s"
        in
        let e2_format =
          match out e2 with
          | Lam _ | App _ -> format_of_string "(%s)"
          | _ -> format_of_string "%s"
        in
        Printf.sprintf
          (e1_format ^^ " " ^^ e2_format)
          (to_string e1) (to_string e2)
end

(* Derived modules for parser *)
module Parsing = struct
  module type PARSING = sig
    type t

    val var : string -> t
    val app : t * t -> t
    val lam : string * t -> t
  end

  module Make (Syntax : SYNTAX) : PARSING with type t = Syntax.t = struct
    open Syntax

    type t = Syntax.t

    let var s = into @@ Var (Var.of_string s)
    let lam (s, e) = into @@ Lam (Var.of_string s, e)
    let app (e1, e2) = into @@ App (e1, e2)
  end

  module Ast : PARSING with type t = Ast.t = Make (Ast)
  module Abt : PARSING with type t = Abt.t = Make (Abt)
end
