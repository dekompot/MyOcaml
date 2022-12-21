module type Numerical_type = sig
  type num_type
  val to_float : num_type -> float
end;;

module type Point =
functor (N : Numerical_type) ->
  sig
   type t
   val distance : t * t -> float
   val create : N.num_type * N.num_type * N.num_type -> t
end;;

module Point_Implementation :Point = functor (N : Numerical_type) -> 
struct
  type t = {x:N.num_type; y:N.num_type; z:N.num_type}
  let distance (m,n) = sqrt((N.to_float(m.x) -. N.to_float(n.x))**2. 
  +. (N.to_float(m.y) -. N.to_float(n.y))**2. +. (N.to_float(m.z) -. N.to_float(n.z))**2.)
  let create (a,b,c) = {x=a;y=b;z=c}
end;;    


module Point_Int = Point_Implementation(struct type num_type = int
                let to_float x = float_of_int(x) end);;
let m = Point_Int.distance(Point_Int.create(0,0,0),Point_Int.create(0,1,1));;

module type Segment = 
functor (N : Numerical_type) ->
sig
  type segment_type
  val length : segment_type -> float
  val create : Point_Implementation(N).t * Point_Implementation(N).t -> segment_type
end;;

module Segment_Implementation = Segment with module N = Numerical_type 
  struct
    type segment_type = {a:Point_Implementation(N).t; b:Point_Implementation(N).t}
    let length points = Point_Implementation(N).distance(points.a, points.b);
    let create (x,y) = {a=x;b=y}
  end;;

module Segment_Int = 
  struct
    type segment_type = {a:Point_Int.t; b:Point_Int.t}
    let length points = Point_Int.distance(points.a, points.b)
    let create (x,y) = {a=x;b=y}
  end;;

let segment1 = Segment_Int.create(Point_Int.create(0,0,0),Point_Int.create(0,1,1));;
Segment_Int.length segment1;;

module type Translate_Point (P:Point) =
struct
  let translate (x,y,z) = 
end;;

module type Make_Point (P:Point) =
  struct
    ssssssdupa
  end
module type Translate =
 sig
  type 
 end;;

module Translate_Point : Translate (P:Point) =
struct
  type 'a t = 
  let translate r = ffw3434;r[0/23]
end