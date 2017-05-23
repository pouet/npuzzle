open Batteries
open Printf


exception InvalidGridSize
exception InvalidGrid
exception InvalidMove

exception NotSolvable
exception Finished


type point = int * int

type node = {
    sz          : int;
    w           : int;
    grid        : int array;
    parent          : point list;
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

    let get_goal size =
        let moves = [
			(1, 0); (0, 1); (-1, 0); (0, -1);
		] in
        let tab = Array.create size 0 in
        let w = int_of_float (sqrt (float_of_int (Array.length grid))) in
		let loop dir (line, col) =
			let ml, mc = moves.(dir) in
			let line', col' = (line + ml, col + mc) in
			if line' < 0 || col' < 0 || line' >= w || col' >= w then loop ((dir + 1) mod 4) (line, col)
			else begin
			end

    let create grid =
        let tmp = {
            sz = Array.length grid;
            w = int_of_float (sqrt (float_of_int (Array.length grid)));
            grid = grid;
            parent = [];
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
            pos = (line', col');
            cost = grid.cost + 1;
        } in
		let prio = heuristic tmp in
        { tmp with prio = prio; f = prio + tmp.cost; parent = (-movl, -movc) :: grid.parent }

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


let iter_neighbors opened closed node neighbors =
    let rec aux opened = function
        | []        -> opened
        | hd :: tl  ->
                    try
                        Hashtbl.find closed node.grid;
                        aux opened tl
                    with Not_found ->
                            let next = Grid.play_move node hd in
                            let opened = Pqueue.add next opened in
                            aux opened tl
    in
    aux opened neighbors

let solved closed node =
    let grid = Grid.create node.grid in
    let acc = ref [ grid ] in
    List.fold_left (fun grid (a, b) ->
        let tmp = Grid.play_move grid (a, b) in
        acc := tmp :: !acc;
        tmp) grid node.parent |> ignore;
    List.iter (fun n -> Grid.print n) !acc;
	raise Finished


let rec astar opened closed =
    if Pqueue.size opened = 0 then raise NotSolvable;

    let node = Pqueue.find_min opened in
    let opened = Pqueue.del_min opened in

    if Grid.is_solved node then solved closed node;

    let neigh = Grid.get_neighbors node in
    let opened = iter_neighbors opened closed node neigh in
	Hashtbl.add closed node.grid node;
	astar opened closed

let solve start =

    let opened = Pqueue.add start Pqueue.empty in
    let closed = Hashtbl.create 1024 in

	try
		astar opened closed
    with
		| Finished -> print_endline "Finished"
		| NotSolvable -> print_endline "Not solvable"
		| InvalidGrid ->  print_endline "Invalid grid"
		| InvalidGridSize ->  print_endline "Invalid grid size"
		| InvalidMove ->  print_endline "Invalid move"
		| Not_found -> print_endline "Not found"

let () =
    let grid = [| 4; 5; 1; 7; 0; 6; 3; 8; 2; |] in
(*
    let grid = [|
        1; 2; 3;
        8; 4; 5;
        7; 6; 0
        |]
    in
*)
(*
    let grid = [|
        1; 6; 0;
        3; 8; 5;
        4; 7; 2
        |]
    in
*)
(*     let grid = [| 0; 4; 1; 8; 3; 2; 6; 7; 5; |] in *)
    let grid = Grid.create grid in
    solve grid
