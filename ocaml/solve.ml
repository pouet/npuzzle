open Printf

(*
 * TODO: faire un module a part
*)
module Pqueue =
struct
    module PrioMap = Map.Make (struct
        (* grid * (line * col) * priority *)
        type t =  int array * (int * int) * int

        let compare (ag, _, app) (bg, _, bpp) =
            if app < bpp then -1
            else if app > bpp then 1
            else begin
                if ag < bg then -1
                else if ag > bg then 1
                else 0
            end
    end)

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
        min, key, PrioMap.remove key t
end

type point = int * int

type node = {
    grid        : int array;
    pos         : point;
    prio        : int;
}

exception InvalidGridSize
exception InvalidGrid

let grid_size grid =
    let w = Array.length grid
    in
    match w with
    | 9     -> 3
    | _     -> raise InvalidGridSize

let print_grid grid =
    let w = grid_size grid in
    print_endline "------";
    Array.fold_left (fun acc x -> printf "%d " x; if acc mod w = w - 1 then
        print_endline ""; acc + 1) 0 grid |> ignore ;
    print_endline "------"


let get_empty_case grid =
    let len = Array.length grid
    and w = grid_size grid in
    let rec aux = function
        | n when n >= len       -> raise InvalidGrid
        | n when grid.(n) = 0   -> n
        | n                     -> aux (n + 1)
    in
    let n = aux 0 in
    (n / w, n mod w)


let is_in_grid grid (line, col) =
    let w = grid_size grid in
    line >= 0 && col >= 0 && line < w && col < w

let is_move_valid grid (line, col) (movl, movc) =
    is_in_grid grid (line + movl, col + movc)

let play_move grid (line, col) (movl, movc) =
    let grid = Array.copy grid
    and w = grid_size grid in
    if is_in_grid grid (line + movl, col + movc)
    then begin
        let tmpl, tmpc = (line + movl, col + movc) in
        let old = line * w + col
        and now = tmpl * w + tmpc in
        let tmp = grid.(old) in
        grid.(old) <- grid.(now);
        grid.(now) <- tmp
    end;
    grid

let undo_move = play_move



let h_manhattan (ax, ay) (bx, by) =
    abs (ax - bx) + abs (ay - by)

let heuristic grid =
    let w = grid_size grid in
    let rec calc acc = function
        | n when n < w      ->
                let a = (grid.(n) / w, grid.(n) mod w)
                and b = (n / w, n mod w) in
                if grid.(n) = 0 then calc acc (n + 1)
                else calc (h_manhattan a b) (n + 1)
        | _                 -> acc
    in
    calc 0 0


let get_neighbors grid (a, b as pt) =
    let moves = [
        (1, 0);
        (0, 1);
        (-1, 0);
        (0, -1);
    ]
    in
    let rec aux = function
        | []            -> []
        | (x, y as mv) :: t   ->
                if is_move_valid grid pt mv
                then { prio = 0; grid = play_move grid pt mv; pos = (a + x, b + y); } :: aux t
                else aux t
    in
    aux moves

(*
    List.fold_left (fun acc mv -> if is_move_valid grid pt mv then
            { grid = play_move grid pt mv; pos = pt + mv; } :: acc else acc) [] moves
*)


(* TODO: rajouter tailles <> 3 *)
let is_solved grid =
    let grid' = [| 1; 2; 3; 4; 5; 6; 7; 8; 0 |]
    in
    grid = grid'

(* TODO: A enlever *)
exception Finished

let solve grid =
    let current = get_empty_case grid
    and opened = Pqueue.create ()
    and closed = Pqueue.create ()
    in

    let start = {
        grid    = grid;
        pos     = current;
        prio    = 0;
    }
    in

    let opened = Pqueue.push (grid, current, 0) start opened
    in

    let rec loop opened closed = 
        if Pqueue.is_empty opened = true then print_endline "finish"
        else begin
(*             if is_solved grid then print_endline "Solved !"; *)

            let current, key, opened = Pqueue.pop_min opened in
(*             let closed = Pqueue.push cost current closed in *)

            let neigh = get_neighbors current.grid current.pos in

            if is_solved current.grid then (print_endline "Solved !"; raise Finished);

            let rec for_each_neigh opened closed = function
                | []        -> opened, closed
                | next :: t ->
(*                         let new_cost = cost + 1 in *)
                        for_each_neigh opened closed t

(*
                        if (Pqueue.exists next closed = false ||
                            new_cost < fst (Pqueue.get next closed))
                        then begin
                            let closed = Pqueue.remove next closed in
                            let closed = Pqueue.push new_cost next closed in
                            let prio = new_cost + heuristic next.grid in
                            let opened = Pqueue.push prio next opened in
                            printf "%d\n" prio;
                            for_each_neigh opened closed t
                        end
                        else 
                            for_each_neigh opened closed t
*)

(*
                        if Pqueue.exists next closed then begin
                            let cost', _ = Pqueue.get next closed in
(*                             printf "%d %d\n" new_cost cost'; *)
                            if new_cost < cost' then begin
                                let prio = new_cost + heuristic next.grid in
                                let opened = Pqueue.push prio next opened in
                                printf "%d\n" prio;
                                for_each_neigh opened closed t
                            end
                            else
                                for_each_neigh opened closed t
                        end
                        else begin
(*
                            let prio = new_cost + heuristic next.grid in
                            let opened = Pqueue.push prio next opened in
*)
(*                             print_grid next.grid; *)
                            for_each_neigh opened closed t
                        end
*)
            in

            let opened, closed = for_each_neigh opened closed neigh
            in

            loop opened closed
        end
    in
    loop opened closed

let _ =
    let grid = [|
        8; 7; 5;
        3; 0; 1;
        4; 2; 6
        |]
    in
(*
    let grid = [|
        1; 2; 3;
        4; 5; 6;
        7; 8; 0
        |]
    in
*)
(*
    let grid' = [|
        8; 7; 5;
        3; 0; 1;
        4; 2; 3
        |]
    in
    if grid = grid' then print_endline "same"
    else print_endline "not same";
*)
    solve grid
