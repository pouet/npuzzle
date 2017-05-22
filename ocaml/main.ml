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



(*
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
*)

end


class my_grid tab' cost' =
    object (self : 'self)
        val _size : int = Array.length tab'
        method w = int_of_float (sqrt (float_of_int (Array.length tab')))
        method tab = tab'
        method empty =
            let rec aux = function
                | n when n >= _size     -> raise InvalidGrid
                | n when self#tab.(n) = 0   -> n
                | n                         -> aux (n + 1)
            in
            let n = aux 0 in
            (n / self#w, n mod self#w)

(*             TODO: A modifier *)
        method goal = [| 1; 2; 3; 8; 0; 4; 7; 6; 5 |]
        method g = cost'
        method h =
            let w = self#w in
            let h_manhattan (ax, ay) (bx, by) =
                abs (ax - bx) + abs (ay - by)
            in
            let search n =
                let rec aux = function
                    | i when i >= _size     -> raise InvalidGrid
                    | i when self#tab.(i) <> n  -> aux (i + 1)
                    | i                         -> i
                in
                aux 0
            in
            let rec calc acc = function
                | n when n < _size      ->
                        let tmp = search self#tab.(n) in
                        printf "[%d-%d]" n tmp;
                        let a = (tmp / w, tmp mod w)
                        and b = (n / w, n mod w) in
                        if self#tab.(n) = 0 then calc acc (n + 1)
                        else calc (acc + h_manhattan a b) (n + 1)
                | _                 -> print_endline ""; acc
            in
            calc 0 0
 

        method is_solved = self#tab = self#goal

        method print =
            let rec aux i =
                printf "%d " self#tab.(i);
                if i mod self#w = self#w - 1 then print_endline "";
                if i < _size - 1 then aux (i + 1)
            in
            print_endline "------";
            aux 0;
            print_endline "------"

        method is_in_grid (line, col) =
            line >= 0 && col >= 0 && line < self#w && col < self#w

        method is_move_valid (movl, movc) =
            self#is_in_grid (fst self#empty + movl, snd self#empty + movc)

        method play_move (movl, movc) =
            let tab' = Array.copy self#tab in
            let line, col = self#empty in
            let line', col' = (line + movl, col + movc) in

            if self#is_move_valid (movl, movc) = false then raise InvalidMove;

            tab'.(line * self#w + col) <- self#tab.(line' * self#w + col');
            tab'.(line' * self#w + col') <- 0;

            new my_grid tab' (self#g + 1)


        method get_neighbors =
            let moves = [
                (1, 0);  (0, 1);
                (-1, 0); (0, -1);
        ] in
            let rec aux = function
                | []        -> []
                | hd :: tl  ->
                        if self#is_move_valid hd then hd :: aux tl
                        else aux tl
            in
            aux moves
    end


module Pqueue = BatHeap.Make (struct
    type t = my_grid
    let compare a b =
        if a#h < b#h then -1
        else if a#h > b#h then 1
        else 0
end)

(* ------------------------------------------- *)

(*
let h_manhattan (ax, ay) (bx, by) =
    abs (ax - bx) + abs (ay - by)

let heuristic grid =
    let search n =
        let rec aux = function
            | i when i >= grid#size -> raise InvalidGrid
            | i when grid.(i) <> n  -> aux (i + 1)
            | i                     -> i
        in
        aux 0
    in
    let rec calc acc = function
        | n when n < w      ->
                let tmp = search grid.(n) in
                let a = (tmp / w, tmp mod w)
                and b = (n / w, n mod w) in
                if grid.(n) = 0 then calc acc (n + 1)
                else calc (h_manhattan a b) (n + 1)
        | _                 -> acc
    in
    calc 0 0

*)
(* ------------------------------------------- *)

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
(*     if Pqueue.size opened = 0 then raise NotSolvable; *)
(*
    let prio, node = Pqueue.find_min opened in
    let opened = Pqueue.del_min opened in
*)

(*     if Grid.is_solved node then raise Finished; *)

(*     let neigh = Grid.get_neighbors node in *)

    ()


let solve grid =
    Grid.print grid;
    let grid = Grid.play_move grid (-1, 0) in
    Grid.print grid;
(*
    let opened = Pqueue.add toto Pqueue.empty in
    let closed = Hashtbl.create 1024 in

    let tata = toto#play_move (0, -1) in
    let tata = toto#play_move (0, -2) in
    let opened = Pqueue.add tata opened in

    let a = Pqueue.find_min opened in
    a#print;
    printf "%d\n" toto#h;
    printf "%d\n" tata#h;
*)

(*     let tab = Grid.create 3 grid (1, 1) in *)
(*     let tab = Grid.play_move tab (0, 1) in *)
(*
    Grid.print tab;

    printf "%B\n" (Grid.is_move_valid tab (0, 0));

    Hashtbl.add closed (Grid.grid tab) tab;
*)



(*
    let prio, grid = Pqueue.find_min opened in
    let a = Hashtbl.find closed grid in
    Hashtbl.remove closed grid;
    Grid.print a;
*)
(*     Array.iter (fun n -> printf "%d " n) grid; *)
(*     printf "\n%d\n" prio; *)
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
