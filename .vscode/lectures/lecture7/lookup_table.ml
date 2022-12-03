type ordering = LT | EQ | GT;;

module type ORDER = 
sig
  type t 
  val compare : t -> t -> ordering
end ;;

module StringOrder : ORDER with type t = string = 
struct
  type t = string 
  let compare s1 s2 = if s1 < s2 then LT else if s1 = s2 then EQ else GT
end;;

module type DICTIONARY = 
sig
  type key 
  type 'a t
  exception DuplicatedKey of key 
  val empty : unit -> 'a t 
  val lookup : 'a t -> key -> 'a option 
  val insert : 'a t -> key * 'a -> 'a t
  val delete : 'a t -> key -> 'a t 
  (*val update : 'a t -> key * 'a -> 'a t*)
end;;

module Dictionary (Key : ORDER) : DICTIONARY with type key = Key.t = 
struct
  type key = Key.t 
  type 'a t = Tip | Node of key * 'a * 'a t * 'a t
  exception DuplicatedKey of key 
  let empty() = Tip 

  let rec lookup tree key = 
    match tree with 
    | Tip -> None
    | Node(k, value, lt, rt) -> match (Key.compare key k) with
                                | EQ -> Some value
                                | LT -> lookup lt key
                                | GT -> lookup rt key
  ;;
  let rec insert tree (key, value) = 
    match tree with 
    | Tip -> Node(key, value, Tip, Tip)
    | Node(k, v, lt, rt) -> match (Key.compare key k) with
            | EQ -> raise (DuplicatedKey key)
            | LT -> Node(k, v, insert lt (key, value), rt)
            | GT -> Node(k, v, lt, insert rt (key, value))

  let rec deletemin tree = 
    match tree with 
    | Node(key, value, Tip, rt) -> (key, value, rt)
    | Tip -> failwith ""
    | Node(k, v, lt, rt) -> let (key, value, subt) = deletemin lt in (k, v, Node(k, v, subt, rt))

  let rec delete tree key = 
    match tree with 
    | Tip -> Tip
    | Node(k, value, lt, rt) -> match (Key.compare key k) with
                                | EQ -> (match (lt, rt) with 
                                        | (_, Tip) -> lt
                                        | (Tip, _) -> rt
                                        | _ -> let (ki, vi, rtree) = deletemin rt in Node(ki, vi, lt, rtree))
                                | LT -> Node(k, value, delete lt key, rt)
                                | GT -> Node(k, value, lt, delete rt key)
  
  end;;

module StringDict = Dictionary(StringOrder);; 

let (<|) t (k, x) = StringDict.insert t (k, x);;

let dict = StringDict.empty();;
let dict = dict <| 
("kot", "cat") <| 
("slon", "elephant") <| 
("pies", "dog") <| 
("ptak", "bird");;

StringDict.lookup dict "pies";;

StringDict.lookup dict "zyrafa";;

dict = StringDict.delete dict "ptak";;

StringDict.lookup dict "ptak";;

