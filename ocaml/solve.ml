open Printf

(*
 * TODO: faire un module a part
*)
module Pqueue =
struct
    module PrioMap = Map.Make (struct type t = int let compare = compare end)

    type t = PrioMap.key

    let empty = PrioMap.empty
    let is_empty = PrioMap.is_empty
    let remove = PrioMap.remove

    let create () = empty

    let exists el map = PrioMap.exists (fun _ el' -> el = el') map
    let filter el map = PrioMap.filter (fun _ el' -> el = el') map
    let get el map = PrioMap.choose (filter el map)

    let push = PrioMap.add
    let pop_min t =
        let key, min = PrioMap.min_binding t in
        min, PrioMap.remove key t
end



type point = int * int

type node = {
    grid        : int array array;
    parent      : int * int;
    pos         : int * int;
    mutable f   : int;
    mutable g   : int;
    mutable h   : int;
}




let get_empty_case grid =
    let w = Array.length grid
    in
    let rec aux lines cols =
        if lines = w then (-1, -1)
        else if cols = w then aux (lines + 1) 0
        else (
            if grid.(lines).(cols) = 0 then (lines, cols)
            else aux lines (cols + 1)
            )
    in
    aux 0 0

let is_in_grid grid (line, col) =
    let w = Array.length grid
    in
    line >= 0 && col >= 0 && line < w && col < w

let is_move_valid grid (line, col) (movl, movc) =
    is_in_grid grid (line + movl, col + movc)

let play_move grid (line, col) (movl, movc) =
    if is_in_grid grid (line + movl, col + movc)
    then (
        let tmpl = line + movl
        and tmpc = col + movc
        and tmp = grid.(line).(col) in
        grid.(line).(col) <- grid.(tmpl).(tmpc);
        grid.(tmpl).(tmpc) <- tmp;
        )

let undo_move = play_move

let h_manhattan (ax, ay) (bx, by) =
    abs (ax - bx) + abs (ay - by)



let get_neighbors grid (l, c as pt) =
    let moves = [|
        (1, 0);
        (0, 1);
        (-1, 0);
        (0, -1);
    |]
    in
    Array.fold_left (fun acc (a, b as mv) -> if is_move_valid grid pt mv = true then
        (l + a, c + b) :: acc else acc) [] moves

(* TODO: rajouter tailles <> 3 *)
let is_solved grid =
    let grid' = [|
        [| 1; 2; 3 |];
        [| 4; 5; 6 |];
        [| 7; 8; 0 |]
        |]
    in
    grid = grid'


let solve grid =
    let current = get_empty_case grid
    and opened = Pqueue.create ()
    and closed = Pqueue.create ()
    in

    let start = {
        grid    = grid;
        parent  = (-1, -1);
        pos     = current;
        f       = 0;
        g       = 0;
        h       = 0
    }
    in

    let opened = Pqueue.push 0 start opened
    in

    let rec loop opened closed = 
        if Pqueue.is_empty opened = true then print_endline "finish"
        else begin
            let (node, opened) = Pqueue.pop_min opened in
            if is_solved grid then print_endline "Solved !";
            loop opened closed
        end
    in
    loop opened closed

(*
        let rec iter_neighbors =
        | [] -> ()
        | h :: t ->
                d
        in
*)

let _ =
    let grid = [|
        [| 8; 7; 5 |];
        [| 3; 0; 1 |];
        [| 4; 2; 6 |]
        |]
    in
(*
    let grid = [|
        [| 1; 2; 3 |];
        [| 4; 5; 6 |];
        [| 7; 8; 0 |]
        |]
    in
*)
(*
    let grid' = [|
        [| 8; 7; 5 |];
        [| 3; 0; 1 |];
        [| 4; 2; 3 |]
        |]
    in
    if grid = grid' then print_endline "same"
    else print_endline "not same";
*)
    solve grid
