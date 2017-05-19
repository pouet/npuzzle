#use "/Users/nchrupal/.opam/system/lib/toplevel/topfind"
#require "batteries"
open Batteries

open Printf


type point = int * int

type node = {
    grid        : int array;
    parent      : int array;
    pos         : point;
    cost        : int; (* g(x) *)
    prio        : int; (* h(x) *)
}

module Pqueue = BatHeap.Make (struct
    type t = int * int array
    let compare (a, _) (b, _) =
        if a < b then -1
        else if a > b then 1
        else 0
end)


module Grid =
struct
    (* width * grid * emptycase *)
    type t = int * int array * point

    exception InvalidGridSize
    exception InvalidGrid
    exception InvalidMove

    let create w grid pt = (w, grid, pt)

    let size (w, _, _) = w * w
    let width (w, _, _) = w
    let grid (_, g, _) = g
    let empty_case (_, _, pt) = pt

    let print (w, grid, _) =
        let sz = w * w in
        let rec aux i =
            printf "%d " grid.(i);
            if i mod w = w - 1 then print_endline "";
            if i < sz - 1 then aux (i + 1)
        in
        print_endline "------";
        aux 0;
        print_endline "------"

    let is_in_grid (w, _, _) (line, col) =
        line >= 0 && col >= 0 && line < w && col < w

    let is_move_valid ((_, _, (line, col)) as grid) (movl, movc) =
        is_in_grid grid (line + movl, col + movc)

    let play_move ((w, grid, (line, col)) as orig) (movl, movc) =
        let grid' = Array.copy grid in
        let line', col' = (line + movl, col + movc) in

        if is_in_grid orig (line', col') = false then raise InvalidMove;
        grid'.(line * w + col) <- grid.(line' * w + col');
        grid'.(line' * w + col') <- 0;
        (w, grid', (line', col'))

    (* TODO: rajouter tailles <> 3 *)
    let is_solved (_, grid, _) =
        let grid' = [| 1; 2; 3; 8; 0; 4; 7; 6; 5 |]
        in
        grid = grid'

    let get_neighbors ((w, grid, (line, col)) as orig) =
        let moves = [
            (1, 0);  (0, 1);
            (-1, 0); (0, -1);
        ] in
        let rec aux = function
            | []        -> []
            | hd :: tl  ->
                    if is_move_valid orig hd then hd :: aux tl
                    else aux tl
        in
        aux moves

end

exception NotSolvable
exception Finished

(*
let iter_neighbors opened closed neighbors =
    let rec aux opened = function
        | []        -> opened
        | hd :: tl  ->
                let cost = 
*)

let rec astar opened closed =
    if Pqueue.size opened = 0 then raise NotSolvable;
    let prio, node = Pqueue.find_min opened in
    let opened = Pqueue.del_min opened in

    if Grid.is_solved node then raise Finished;

    let neigh = Grid.get_neighbors node in

    ()


let solve grid =
    let opened = Pqueue.add (0, grid) Pqueue.empty in
    let closed = Hashtbl.create 1024 in

    let tab = Grid.create 3 grid (1, 1) in
(*     let tab = Grid.play_move tab (0, 1) in *)
    Grid.print tab;

    printf "%B\n" (Grid.is_move_valid tab (0, 0));

    Hashtbl.add closed (Grid.grid tab) tab;



    let prio, grid = Pqueue.find_min opened in
    let a = Hashtbl.find closed grid in
    Hashtbl.remove closed grid;
    Grid.print a;
(*     Array.iter (fun n -> printf "%d " n) grid; *)
(*     printf "\n%d\n" prio; *)
    ()

let () =
    let grid = [| 4; 5; 1; 7; 0; 6; 3; 8; 2; |] in
    solve grid
