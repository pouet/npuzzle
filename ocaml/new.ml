open Printf

(*
 * Module Pqueue handle a priority queue
 * The module MUST be mutable
 *
 * -> create : create an empty Pqueue
*)

module Pqueue = struct
    exception Pqueue_empty

    let create =
        []

    let is_empty = function
        | []    -> true
        | _     -> false

    let rec is_present f el = function
        | []             -> false
        | (_, e) :: t    ->
                if f el e = true then true
                else is_present f el t

    let pop = function
        | (_, e) :: t    -> (e, ref t)
        | []        -> raise Pqueue_empty

    let rec push prio el = function
        | []                -> (prio, el) :: []
        | (p, e as h) :: t  ->
                if prio <= p then (prio, el) :: h :: t
                else h :: push prio el t

    let rec print f = function
        | []              -> ()
        | (p, e) :: t     -> begin
                Printf.printf "%d : %s\n" p (f e);
                print f t
        end
end

type point = int * int

type node = {
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

let undo_move grid src move =
    play_move grid src move

let h_manhattan (ax, ay) (bx, by) =
    abs (ax - bx) + abs (ay - by)

(*
 * current -> int * int | empty case
 * opened  -> 
*)

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
    and opened = ref Pqueue.create
    and closed = ref Pqueue.create
    in

    let start = {
        parent  = (-1, -1);
        pos     = current;
        f       = 0;
        g       = 0;
        h       = 0;
    }
    in


    opened := Pqueue.push 0 start !opened;

    let ok, opened = Pqueue.pop !opened
    in

    printf "%d\n" (fst ok.parent);

    let neigh = get_neighbors grid current
    in

    List.iter (fun (a, b) -> printf "|> %d %d\n" a b) neigh;

    while Pqueue.is_empty !opened = false do
        let tmp, opened = Pqueue.pop !opened in
        closed := Pqueue.push tmp.h tmp !closed
    done

(*
    let rec loop opened closed pt = 
        if Pqueue.is_empty opened = false then begin
            let (l, c as pt), opened = Pqueue.pop opened in
            if is_solved grid then print_endline "Wahou !";
            loop opened closed
        end
    in
    loop opened closed;
*)
(*
    let start = {
        parent  = grid;
        f       = 0;
        g       = 0;
        h       = h_manhattan (0, 0) (1, 2);
    }
    in
*)

(*
    let opened = Pqueue.push 5 (21, 42) opened in
    let opened = Pqueue.push 6 (12, 98) opened in
    let opened = Pqueue.push 1 (0, 3) opened in
    let opened = Pqueue.push 2 (55, 1234) opened in
*)



(*
    Pqueue.print (fun (a, b) -> string_of_int a ^ " " ^ string_of_int b) opened;
*)

(*
    let (n, m), opened = Pqueue.pop opened in
    printf "%d %d\n" n m;

    if Pqueue.is_empty opened then print_endline "empty"
    else print_endline "not empty";
    if Pqueue.is_present (fun el e -> el = e) (55, 1234) opened then print_endline "present"
    else print_endline "not present"
*)


(*
        printf "%d %d\n" (fst empty) (snd empty);
        play_move grid empty moves.(0);
        let empty = get_empty_case grid
        in
        printf "%d %d\n" (fst empty) (snd empty);
        play_move grid empty moves.(0);
        let empty = get_empty_case grid
        in
        printf "%d %d\n" (fst empty) (snd empty)
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
