module Var :
sig 
  type t
  [@@deriving compare, sexp]

  val of_string : string -> t
  val to_string : t -> string

  include Core.Comparable.S with type t := t

  module Map : Core.Map.S with type Key.t = t
                            and type Key.comparator_witness = comparator_witness
  module Set : Core.Set.S with type Elt.t = t
                            and type Elt.comparator_witness = comparator_witness

  val equal: t -> t -> bool
  val fresh : unit -> t
end  =
struct
  include Core.String
  let count = ref 0
  let fresh () = let t = !count in count := t + 1; ("#x" ^ Int.to_string t)
end

(* Top-level commands *)
type 'a cmd = 
  | Eval  of 'a
  | Test  of 'a * 'a 
  | Print of 'a

(* Representation of the program *)
module type SYNTAX = 
sig
  type t

  type view = 
    | Var of Var.t
    | Lam of Var.t * t
    | App of t * t

  val into : view -> t
  val out : t -> view
  val subst : t * Var.t -> t -> t
  val aequiv: t -> t -> bool
  val to_string : t -> string
end

module Ast = 
struct
  type t = 
    | Var of Var.t
    | Lam of Var.t * t
    | App of t * t
  
  type view = t = 
    | Var of Var.t
    | Lam of Var.t * view
    | App of view * view

  let into t = t

  let out t = t

  (* [e'/x]e, that is, Substitute e' for x in e *)
  let rec subst (e', x) e = failwith "Unimplemented"

  (* Test for alpha equivalence between e1 and e2 *)
  let rec aequiv e1 e2 = failwith "Unimplemented"

  let rec to_string e = 
    match e with
    | Var v -> Var.to_string v
    | Lam (x, e) ->
      Printf.sprintf "\\%s.%s" (Var.to_string x) (to_string e)
    | App (e1, e2) ->
      let e1_format =
        match e1 with
        | Lam _ -> format_of_string "(%s)"
        | _     -> format_of_string "%s"
      in
      let e2_format =
        match e2 with
        | Lam _ | App _ -> format_of_string "(%s)"
        | _             -> format_of_string "%s"
      in
      Printf.sprintf (e1_format ^^ " " ^^ e2_format) (to_string e1) (to_string e2)
end

module Abt =
struct
  type t = 
    | BVAR of int
    | FVAR of Var.t
    | LAM  of t
    | APP  of t * t

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
   *          <--INTO-- Lam (x, FVAR x) 
   *
   *   LAM (LAM (APP #0 #1)) ---OUT--> 
   *                         <--INTO-- Lam (x, (LAM (APP #0 (FVAR x))))
   *    *)
  type view = 
    | Var  of Var.t
    | Lam  of Var.t * t
    | App  of t * t

  (* [e'/x] e, that is, Substitute e' for x in e*)
  let rec subst (e', x) e = failwith "Unimplemented"

  (* Test for alpha equivalence between e1 and e2 *)
  let rec aequiv e1 e2 = failwith "Unimplemented"

  (* The following are two helper function you probably would need *)

  (* binds free variable given by Var.t *)
  let bind : Var.t * t -> t = failwith "Unimplemented"

  (* unbinds the the top most bounded variable by freeing it as Var.t*)
  let unbind : t -> Var.t * t = failwith "Unimplemented"

  let into : view -> t = failwith "Unimplemented"

  let out  : t -> view = failwith "Unimplemented"

  let rec to_string e =
    match out e with 
    | Var v -> Var.to_string v
    | Lam (x, e) -> 
      Printf.sprintf "\\%s.%s" (Var.to_string x) (to_string e)
    | App (e1, e2) -> 
      let e1_format = 
        match out e1 with 
        | Lam _ -> format_of_string "(%s)"
        | _     -> format_of_string "%s"
      in
      let e2_format = 
        match out e2 with 
        | Lam _ | App _ -> format_of_string "(%s)"
        | _             -> format_of_string "%s"
      in
      Printf.sprintf (e1_format ^^ " " ^^ e2_format) (to_string e1) (to_string e2)
end

(* Derived modules for parser *)
module Parsing = 
struct 
  module type PARSING =
  sig
    type t
    val var  : string     -> t
    val app  : t * t      -> t
    val lam  : string * t -> t
  end

  module Make (Syntax : SYNTAX) : PARSING with type t = Syntax.t = 
  struct
    open Syntax

    type t = Syntax.t

    let var s        = into @@ Var (Var.of_string s)
    let lam (s, e)   = into @@ Lam (Var.of_string s, e)
    let app (e1, e2) = into @@ App (e1, e2)
  end

  module Ast : PARSING with type t = Ast.t = Make(Ast)
  module Abt : PARSING with type t = Abt.t = Make(Abt)
end