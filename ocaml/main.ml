(*
#use "/Users/nchrupal/.opam/system/lib/toplevel/topfind"
#require "batteries"
*)
open Batteries

open Printf


exception InvalidGridSize
exception InvalidGrid
exception InvalidMove

type point = int * int

type node = {
    sz          : int;
    w           : int;
    grid        : int array;
    parent      : int array;
    goal        : int array;
    pos         : point; (* empty case *)
    cost        : int; (* g(x) *)
    prio        : int; (* h(x) *)
	f			: int; (* f(x) *)
}

let h_manhattan (ax, ay) (bx, by) =
    abs (ax - bx) + abs (ay - by)

let search grid n =
    let rec aux = function
        | i when i >= grid.sz        -> raise InvalidGrid
        | i when grid.goal.(i) <> n  -> aux (i + 1)
        | i                          -> (i / grid.w, i mod grid.w)
    in
    aux 0

let heuristic grid =
    let rec calc acc = function
        | n when n < grid.sz    ->
                let a = search grid grid.grid.(n)
                and b = (n / grid.w, n mod grid.w) in
(*                 printf "[%d (%d-%d)-(%d-%d)]\n" grid.grid.(n) (fst a) (snd a)
 *                 (fst b) (snd b); *)
                if grid.grid.(n) = 0 then calc acc (n + 1)
                else calc (acc + h_manhattan a b) (n + 1)
        | _                     -> acc
    in
    calc 0 0

module Grid =
struct
    (* width * grid * emptycase *)

    type t = node


    let get_empty_case grid =
        let len = Array.length grid
        and w = int_of_float (sqrt (float_of_int (Array.length grid))) in
        let rec aux = function
            | n when n >= len       -> raise InvalidGrid
            | n when grid.(n) = 0   -> n
            | n                     -> aux (n + 1)
        in
        let n = aux 0 in
        (n / w, n mod w)

    let create grid =
        let tmp = {
            sz = Array.length grid;
            w = int_of_float (sqrt (float_of_int (Array.length grid)));
            grid = grid;
            parent = grid;
(*             goal = [| 1; 2; 3; 4; 5; 6; 7; 8; 0 |]; *)
            goal = [| 1; 2; 3; 8; 0; 4; 7; 6; 5 |];
            pos = get_empty_case grid;
            cost = 0;
            prio = 0;
			f = 0;
        }
        in
		let prio = heuristic tmp in
        { tmp with prio = prio; f = prio; }

    let print grid =
        let rec aux i =
            printf "%d " grid.grid.(i);
            if i mod grid.w = grid.w - 1 then print_endline "";
            if i < grid.sz - 1 then aux (i + 1)
        in
        print_endline "------";
        printf "prio : %d | cost : %d\n" grid.prio grid.cost;
        aux 0;
        print_endline "------"

    let is_in_grid grid (line, col) =
        line >= 0 && col >= 0 && line < grid.w && col < grid.w

    let is_move_valid grid (movl, movc) =
        let line, col = grid.pos in
        is_in_grid grid (line + movl, col + movc)

    let play_move grid (movl, movc) =
        let grid' = Array.copy grid.grid in
        let line, col = grid.pos in
        let line', col' = (line + movl, col + movc) in

        if is_in_grid grid (line', col') = false then raise InvalidMove;
        grid'.(line * grid.w + col) <- grid.grid.(line' * grid.w + col');
        grid'.(line' * grid.w + col') <- 0;
        let tmp = { grid with
            grid = grid';
            parent = grid.grid;
            pos = (line', col');
            cost = grid.cost + 1;
        } in
		let prio = heuristic tmp in
        { tmp with prio = prio; f = prio + tmp.cost; }

    let is_solved grid =
        grid.grid = grid.goal

    let get_neighbors grid =
        let moves = [
            (1, 0);  (0, 1);
            (-1, 0); (0, -1);
        ] in
        let rec aux = function
            | []        -> []
            | hd :: tl  ->
                    if is_move_valid grid hd then hd :: aux tl
                    else aux tl
        in
        aux moves

end

module Pqueue = BatHeap.Make (struct
    type t = node
    let compare a b =
        if a.f < b.f then -1
        else if a.f > b.f then 1
        else 0
end)


exception NotSolvable
exception Finished

(*
1 	Create a node containing the goal state node_goal
2 	Create a node containing the start state node_start
3 	Put node_start on the open list
4 	while the OPEN list is not empty
5 	{
6 	Get the node off the open list with the lowest f and call it node_current
7 	if node_current is the same state as node_goal we have found the solution; break from the while loop
8 	    Generate each state node_successor that can come after node_current
9 	    for each node_successor of node_current
10 	    {
11 	        Set the cost of node_successor to be the cost of node_current plus the cost to get to node_successor from node_current
12 	        find node_successor on the OPEN list
13 	        if node_successor is on the OPEN list but the existing one is as good or better then discard this successor and continue
14 	        if node_successor is on the CLOSED list but the existing one is as good or better then discard this successor and continue
15 	        Remove occurences of node_successor from OPEN and CLOSED
16 	        Set the parent of node_successor to node_current
17 	        Set h to be the estimated distance to node_goal (Using the heuristic function)
18 	         Add node_successor to the OPEN list
19 	    }
20 	    Add node_current to the CLOSED list
21 	} 
*)

let iter_neighbors opened closed node neighbors =
	let opened' = Pqueue.to_list opened in
(*
	let rec exist node = function
		| []		-> false
		| hd :: tl	->
					if hd.grid = node.grid && hd.f < node.f then true
					else exist node tl
	in
*)
(*
    let rec toto next = function
		| []		-> None
		| hd :: tl	->
					if hd.grid = node.grid && hd.f < node.f then Some hd
					else toto node tl
	in
*)

    let rec del_one node = function
        | []        -> []
        | hd :: tl  ->
                if hd.grid = node.grid then del_one node tl
                else hd :: del_one node tl
    in

	let get_opened node =
        try
            Some (List.find (fun n -> n.grid = node.grid && n.f < node.f) opened')
        with Not_found  -> None
	in

    let get_closed node =
        try
            let tmp = Hashtbl.find closed node.grid in
            if tmp.f < node.f then raise Not_found;
            Some tmp
        with Not_found -> None
    in

    let remove_closed closed node =
        match node with
        | None      -> ()
        | Some n    -> Hashtbl.remove closed n.grid
    in

    let remove_opened opened node =
        match node with
        | None      -> opened
        | Some n    -> Pqueue.of_list (del_one n (Pqueue.to_list opened))
    in

    (* 9 *)
    let rec aux opened = function
        | []        -> opened
        | hd :: tl  ->
                (* 11 *)
				let next = Grid.play_move node hd in
                (* 12/13/14 *)
                let op = get_opened next in
                let cl = get_closed next in
                (* 15 *)
                let opened = remove_opened opened op in
(*                 printf "(%d" (Hashtbl.length closed); *)
                remove_closed closed cl;
(*                 printf " %d)" (Hashtbl.length closed); *)
                (* 18 *)
                let opened = Pqueue.add next opened in
                aux opened tl

(*
                try
                    let tmp = Hashtbl.find closed next.grid in
					if next.cost < tmp.cost then raise Not_found;
					if exist next opened' = false then raise Not_found;
					aux opened tl
                with Not_found ->
					let opened = Pqueue.add next opened in
					aux opened tl
*)
    in
    aux opened neighbors
(*         with Not_found -> print_endline "PUTAIN NOT FOUND !!"; opened *)

let solved closed node =
	let rec get_answer parent =
		if parent.grid <> parent.parent then begin
			Grid.print parent;
(*             printf "%d\n" (Hashtbl.length closed); *)
			let node = Hashtbl.find closed parent.parent in
 			get_answer node;
			print_endline "------------------------------------------";
(* 			Grid.print node; *)
			Grid.print parent;
			print_endline "------------------------------------------";
		end
		else
			Grid.print parent
	in
	get_answer node;
    print_endline "Finished !";
	raise Finished


let rec astar opened closed =
    (* 4 *)
    if Pqueue.size opened = 0 then raise NotSolvable;

(*  	printf "(%d %d)" (Pqueue.size opened) (Hashtbl.length closed); *)

    (* 5/6 *)
    let node = Pqueue.find_min opened in
    let opened = Pqueue.del_min opened in

    (* 7 *)
    if Grid.is_solved node then solved closed node;

    (* 8 *)
    let neigh = Grid.get_neighbors node in
    (* 9/10 *)
    let opened = iter_neighbors opened closed node neigh in
    (* 20 *)
	Hashtbl.add closed node.grid node;
	astar opened closed

let solve start =

    (* 3 *)
    let opened = Pqueue.add start Pqueue.empty in
    let closed = Hashtbl.create 1024 in

    astar opened closed

(*
	try
		astar opened closed
    with
		| Finished -> print_endline "Finished"
		| NotSolvable -> print_endline "Not solvable"
		| InvalidGrid ->  print_endline "Invalid grid"
		| InvalidGridSize ->  print_endline "Invalid grid size"
		| InvalidMove ->  print_endline "Invalid move"
		| Not_found -> print_endline "Not found"
		| _ -> print_endline "error"
*)

let () =
(*     let grid = [| 4; 5; 1; 7; 0; 6; 3; 8; 2; |] in *)
(*
    let grid = [|
        1; 2; 3;
        8; 4; 5;
        7; 6; 0
        |]
    in
*)
    let grid = [|
        1; 6; 0;
        3; 8; 5;
        4; 7; 2
        |]
    in
(*    let grid = [| 0; 4; 1; 8; 3; 2; 6; 7; 5; |] in *)
    (* 1-2 *)
    let grid = Grid.create grid in
    solve grid
