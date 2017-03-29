open Printf

module Pqueue = struct
    exception Pqueue_empty

    let create =
        []

    let is_empty = function
        | []    -> true
        | _     -> false

    let pop = function
        | (_, e) :: t    -> (e, t)
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
    grid    : int array array;
    f       : int;
    g       : int;
    h       : int;
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

let print_el (a, b) =
    string_of_int a ^ " " ^ string_of_int b

let solve grid =
    let empty = get_empty_case grid
    and moves = [|
        (1, 0);
        (0, 1);
        (-1, 0);
        (0, -1);
    |]
    in

    let opened = Pqueue.create
    and closed = Pqueue.create
    in

    let opened = Pqueue.push 5 (21, 42) opened in
    let opened = Pqueue.push 6 (12, 98) opened in
    let opened = Pqueue.push 1 (0, 3) opened in
    let opened = Pqueue.push 2 (55, 1234) opened in

    Pqueue.print print_el opened;

    let tmp = Pqueue.pop opened in
    let opened = snd tmp
    and n, m = fst tmp
    in
    printf "%d %d\n" n m;

    if Pqueue.is_empty opened then print_endline "empty"
    else print_endline "not empty";

        (*
let opened = Hashtbl.create 997
and closed = Hashtbl.create 997
in

let a 

let start = {
    grid    = grid;
    f       = 0;
    g       = 1;
    h       = 0;
}
in
*)


        printf "%d %d\n" (fst empty) (snd empty);
        play_move grid empty moves.(0);
        let empty = get_empty_case grid
        in
        printf "%d %d\n" (fst empty) (snd empty);
        play_move grid empty moves.(0);
        let empty = get_empty_case grid
        in
        printf "%d %d\n" (fst empty) (snd empty)

let _ =
    let grid = [|
        [| 8; 7; 5 |];
        [| 3; 0; 1 |];
        [| 4; 2; 6 |]
        |]
        in
        solve grid
