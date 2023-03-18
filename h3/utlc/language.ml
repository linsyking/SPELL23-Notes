module type DYNAMICS = sig
  type t

  val step : t -> t option
  val norm : t -> t
end

module MakeDynamics (S : Syntax.SYNTAX) : DYNAMICS with type t = S.t = struct
  open S

  type t = S.t

  let rec step : t -> t option =
   fun e ->
    match out e with
    | Var _ -> None
    | Lam (x, y) -> (
        match step y with Some z -> Some (into (Lam (x, z))) | None -> None)
    | App (x, y) -> (
        match out x with
        | Var _ -> (
            match step y with
            | Some z -> Some (into (App (x, z)))
            | None -> None)
        | App (w, k) -> (
            match step w with
            | Some z -> Some (into (App (into (App (z, k)), y)))
            | None -> None)
        | Lam (w, k) -> Some (subst (y, w) k))

  let rec norm x = match step x with None -> x | Some y -> norm y
end

module Dynamics = struct
  module Ast : DYNAMICS with type t = Syntax.Ast.t = MakeDynamics (Syntax.Ast)
  module Abt : DYNAMICS with type t = Syntax.Abt.t = MakeDynamics (Syntax.Abt)
end
