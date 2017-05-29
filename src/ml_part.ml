(* open Batteries *)
open Printf


exception InvalidGridSize
exception InvalidGrid
exception InvalidMove

exception NotSolvable
exception Finished of string


type point = int * int

type node = {
    sz          : int;
    w           : int;
    grid        : int array;
    parent      : point list;
    goal        : int array;
    pos         : point; (* empty case *)
    cost        : int; (* g(x) *)
    prio        : int; (* h(x) *)
	f			: int; (* f(x) *)
}

(*
    2 tableau lignes / colonnes
    On compte la distance pour chaque ligne et colonne en fonction de sa
    position pour aller a la destination
    Une fois comptee, on verifie que pour chaque ligne et colonne les 2 overlap
    ou pas
*)

let linear_conflict grid =
    let cols = Array.make grid.sz 0
    and rows = Array.make grid.sz 0
    and w = grid.w
    in
    let array_index grid sz n =
        let rec aux i =
            if i >= sz then raise Not_found;
            if grid.(i) = n then i
            else aux (i + 1)
        in
        aux 0
    in
    let rec cpt n =
        if n < grid.sz then begin
            let idx = array_index grid.grid grid.sz n
            and idx' = array_index grid.goal grid.sz n in
            let line, col = (idx / w, idx mod w)
            and line', col' = (idx' / w, idx' mod w) in
            if line = line' then rows.(idx) <- col' - col;
            if col = col' then cols.(col * w + line) <- line' - line;
            cpt (n + 1)
        end
    in
    cpt 0;
    let count acc n =
        let rec count' acc i j =
            if i >= w then acc
            else if j >= w then count' acc (i + 1) 0
            else begin
                let cnt = rows.(i) + rows.(j) in
                let acc = acc + (if rows.(i) <> 0 && cnt = 0 then 1 else 0) in
                let cnt = cols.(i) + cols.(j) in
                let acc = acc + (if cols.(i) <> 0 && cnt = 0 then 1 else 0) in
(*                 if rows.(i) <> 0 && cnt = 0 then printf "collision : [%d %d] (%d, %d) %d\n" i j rows.(i) rows.(j) cnt; *)
(*                 let cnt = cols.(i) + cols.(j) in *)
(*                 printf "%d %d - %4d %4d\n" i j cols.(i) cols.(j); *)
(*                 if cols.(i) <> 0 && cnt = 0 then printf "collision : [%d %d] (%d, %d) %d\n" i j cols.(i) cols.(j) cnt; *)
                count' acc i (j + 1)
            end
        in
        if n < w then count' 0 0 0
        else acc
    in
    count 0 0

(*
let l_conflict grid =
    let goal = grid.goal in
    let sz = grid.sz
*)


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
    calc 0 0 + 2 * linear_conflict grid

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
        let moves = [| (0, 1); (1, 0); (0, -1); (-1, 0); |] in
        let tab = Array.create size 0 in
        let w = int_of_float (sqrt (float_of_int size))
        in
		let rec loop dir n (line, col) =
			let ml, mc = moves.(dir) in
			let line', col' = (line + ml, col + mc) in
			if n < size then begin
                tab.(line * w + col) <- n;
                if line' < 0 || col' < 0 || line' >= w || col' >= w || tab.(line' * w + col') <> 0 then
                    loop ((dir + 1) mod 4) n (line, col)
                else
                    loop dir (n + 1) (line', col')
			end
            else tab
        in
        loop 0 1 (0, 0)

    let create grid =
        let tmp = {
            sz = Array.length grid;
            w = int_of_float (sqrt (float_of_int (Array.length grid)));
            grid = grid;
            parent = [];
            goal = get_goal (Array.length grid);
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
    let res = ref "" in

    let get_dir = function
        | (1, 0)    -> "B"
        | (-1, 0)   -> "H"
        | (0, 1)    -> "D"
        | (0, -1)   -> "G"
        | _         -> raise InvalidMove
    in

    List.fold_left (fun grid (a, b) ->
        let tmp = Grid.play_move grid (a, b) in
        acc := tmp :: !acc;
        res := (get_dir (a, b)) ^ !res;
        tmp) grid node.parent |> ignore;
    List.iter (fun n -> Grid.print n) !acc;
    print_endline !res;
	raise (Finished !res)


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
        | Finished res -> print_endline ("Finished with : " ^ res); res
        | _ -> ""
(*
		| NotSolvable -> print_endline "Not solvable"
		| InvalidGrid ->  print_endline "Invalid grid"
		| InvalidGridSize ->  print_endline "Invalid grid size"
		| InvalidMove ->  print_endline "Invalid move"
		| Not_found -> print_endline "Not found"
*)

(* let c_interface : int array -> int -> string = fun grid heuristic -> *)
let ocaml_interface grid heuristic =
(*
    Array.iter (fun x -> printf "%d " x) grid;
    printf "\n%d\n" heuristic;
    "HBGD"
*)
    let grid = Grid.create grid in
    solve grid


(*
let ocaml_interface () =
    print_endline "Hello World"
*)

let () =
      Callback.register "Ocaml interface" ocaml_interface

(*
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
*)
