open Printf

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
                if lines = w
                then (-1, -1)
                else if cols = w
                then aux (lines + 1) 0
                else (
                        if grid.(lines).(cols) = 0
                        then (lines, cols)
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

let solve grid =
        let empty = get_empty_case grid
        and moves = [|
                (1, 0);
                (0, 1);
                (-1, 0);
                (0, -1);
        |]
        in

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
