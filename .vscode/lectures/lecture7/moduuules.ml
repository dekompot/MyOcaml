module type T = sig
  type t
  val x : t
end

module Pair1 (M : T) = struct
  type a = M.t
  let p = (M.x, 1)
end

