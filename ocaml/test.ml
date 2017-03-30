(*
module type X_int =
sig
	val x : int
end

module Increment (M : X_int) : X_int =
struct
    let x = M.x + 1
end

module Three =
struct
    let x = 3
end

module Four = Increment(Three)
*)

(*
module type Comparable = sig
    type t
    val compare : t -> t -> int
end

module Make_interval(EndPoint : Comparable) = struct
    type t =
        | Interval of EndPoint.t * EndPoint.t
        | Empty

    let create low high =
        if EndPoint.compare low high > 0 then Empty
        else Interval (low, high)

    let is_empty = function
        | Empty         -> true
        | Interval _    -> false

    let contains t x =
        match t with
        | Empty             -> false
        | Interval (l, h)   ->
                EndPoint.compare x l >= 0 && EndPoint.compare x h <= 0

end

module Int_interval =
    Make_interval(struct
        type t = int
        let compare = compare
    end)

module Toto = Make_interval(String)
*)

(*
(* Input signature for the functor Map.MakeFn *)
module type EQUAL =
sig
    type t
    val equal : t -> t -> bool
end

(* Input signature for the functor Map.MakeFn *)
module type VALUE =
sig
    type t
end

(* Output signature for the functor Map.MakeFn *)
module type S =
sig
    type t
    type key
    type value

    exception NotFound

    val empty : t
    val add : key -> value -> t -> t
    val find : key -> t -> value
end

(* Functor that returns a map implementation given an equality implementation *)
module MakeFn (Equal : EQUAL) (Value : VALUE)
    : (S with type key = Equal.t with type value = Value.t) =
struct
    type key = Equal.t
    type value = Value.t
    type t = (key * value) list

    exception NotFound

    let empty = []
    let add k v m = (k, v) :: m
    let rec find k m = match m with
        | [] -> raise NotFound
        | (k', v') :: m' ->
                if Equal.equal k k' then v'
                else find k m'
end
*)

(*
module Ord =
struct
    type t = int
    let compare = compare
end

module TestMap = Map.Make(Ord)

let a = TestMap.add 0 "zero" TestMap.empty;

()
*)

module Pqueue =
struct
    module PrioMap = Map.Make (struct type t = int let compare = compare end)

    type t = PrioMap.key

    let empty = PrioMap.empty
    let is_empty = PrioMap.is_empty
    let remove = PrioMap.remove

    let exists el map = PrioMap.exists (fun _ el' -> el = el') map
    let filter el map = PrioMap.filter (fun _ el' -> el = el') map
    let get el map = PrioMap.choose (filter el map)

    let push = PrioMap.add
    let pop_min t =
        let key, min = PrioMap.min_binding t in
        min, PrioMap.remove key t
end

(*
# module Pqueue = struct
  module PrioMap = Map.Make (struct type t = int let compare = compare end)
  type t = PrioMap.key
  let push = PrioMap.add
  let pop_min t = let key, min = PrioMap.min_binding t in min, PrioMap.remove key t
  end;;
module Pqueue :
  sig
    module PrioMap :
      sig
        type key = int
        type +'a t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val max_binding : 'a t -> key * 'a
        val choose : 'a t -> key * 'a
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
      end
    type t = PrioMap.key
    val push : PrioMap.key -> 'a -> 'a PrioMap.t -> 'a PrioMap.t
    val pop_min : 'a PrioMap.t -> 'a * 'a PrioMap.t
  end
*)
