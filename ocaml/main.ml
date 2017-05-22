(*
#use "/Users/nchrupal/.opam/system/lib/toplevel/topfind"
#require "batteries"
*)
open Batteries

open Printf


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

    exception InvalidGridSize
    exception InvalidGrid
    exception InvalidMove

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
        }
        in
        { tmp with prio = heuristic tmp }

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
        { tmp with prio = heuristic tmp }

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
        if a.prio < b.prio then -1
        else if a.prio > b.prio then 1
        else 0
end)


exception NotSolvable
exception Finished


let iter_neighbors opened closed neighbors =
    let rec aux opened = function
        | []        -> opened
        | hd :: tl  ->
                try
(*                    TODO: continuer   *)
                    let next = Grid.play_move 
                    Hashtbl.find closed hd.grid;
                    aux opened tl
                with Not_found -> aux opened tl
    in
    aux opened neighbors


let rec astar opened closed =
    if Pqueue.size opened = 0 then raise NotSolvable;

    let node = Pqueue.find_min opened in
    let opened = Pqueue.del_min opened in

    if Grid.is_solved node then raise Finished;

    let neigh = Grid.get_neighbors node in
    let opened = iter_neighbors opened closed neigh in
    ()


let solve grid =

    let opened = Pqueue.add grid Pqueue.empty in
    let closed = Hashtbl.create 1024 in

    Hashtbl.add closed grid.grid grid;

(*
    Grid.print grid;
    let grid = Grid.play_move grid (-1, 0) in
    Grid.print grid;
*)

    let grid = Pqueue.find_min opened in
    let a = Hashtbl.find closed grid.grid in
    Hashtbl.remove closed grid.grid;
    Grid.print a;

    ()

let () =
(*     let grid = [| 4; 5; 1; 7; 0; 6; 3; 8; 2; |] in *)
    let grid = [|
        1; 2; 3;
        8; 4; 5;
        7; 6; 0
        |]
    in
    let grid = Grid.create grid in
    solve grid
