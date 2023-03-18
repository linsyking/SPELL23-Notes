module type DYNAMICS = sig
  type t

  val step : t -> t option
  val norm : t -> t
end

module MakeDynamics (S : Syntax.SYNTAX) : DYNAMICS with type t = S.t = struct
  open S

  type t = S.t

  let step : t -> t option = failwith "Unimplemented (optional)"
  let norm : t -> t = failwith "Unimplemented"
end

module Dynamics = struct
  module Ast : DYNAMICS with type t = Syntax.Ast.t = MakeDynamics (Syntax.Ast)
  module Abt : DYNAMICS with type t = Syntax.Abt.t = MakeDynamics (Syntax.Abt)
end
