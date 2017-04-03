open Printf

(*
 * TODO: faire un module a part
*)
module Pqueue =
struct
    module PrioMap = Map.Make (struct
        type t = int array
        let compare = compare
    end)

    type t = PrioMap.key

    let empty = PrioMap.empty
    let is_empty = PrioMap.is_empty
    let remove = PrioMap.remove
    let get = PrioMap.find

    let create () = empty

    let exists key map = PrioMap.exists (fun key' _ -> key = key') map

    let push = PrioMap.add
    let pop_min t =
        let key, min = PrioMap.min_binding t in
        key, min, PrioMap.remove key t
end

type point = int * int

type node = {
    grid        : int array;
    parent      : int array;
    pos         : point;
    cost        : int; (* g(x) *)
    prio        : int; (* h(x) *)
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


(* ============================== *)

exception NotFound

let array_index tab n =
    let w = Array.length tab in
    let rec aux i =
        if i >= w then raise NotFound;
        if tab.(i) = n then i
        else aux (i + 1)
    in
    aux 0

let linear_conflict grid =
(*     let grid' = [| 1; 2; 3; 4; 0; 5; 6; 7; 8 |] in *)
    let grid' = [| 1; 2; 3; 8; 0; 4; 7; 6; 5 |] in
    let w = grid_size grid in
    let cols = Array.make 9 0
    and rows = Array.make 9 0
    in
    let rec cpt = function
        | n when n < w * w  ->
                let idx = array_index grid n
                and idx' = array_index grid' n in
                let line, col = (idx / w, idx mod w)
                and line', col' = (idx' / w, idx' mod w) in
(*                 printf "%d -> [%d %d] (%d, %d) (%d %d)\n" n idx idx' line col line' col'; *)
                if line = line' then rows.(idx) <- col' - col;
                if col = col' then cols.(col * w + line) <- line' - line;
                cpt (n + 1)
        | _             -> ()
    in
    cpt 0;
    let rec count acc n =
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
(*     printf "conflicts : %d\n" (count 0 0); *)
(*
    (* chaque ligne *)
    for i = 0 to w - 1 do
        (* chaque case et case + 1 *)
        for j = i * w to (i + 1) * w - 1 do
            for k = j + 1 to (i + 1) * w - 1 do
                let a = rows.(j) + rows.(k) in
                printf "%d %d - %4d %4d\n" i j rows.(j) rows.(k);
                if rows.(j) <> 0 && a = 0 then printf "collision : [%d %d] (%d, %d) %d\n" j k grid.(j) grid.(k) a;
                let a = cols.(j) + cols.(k) in
                if cols.(j) <> 0 && a = 0 then printf "collision : [%d %d] (%d, %d) %d\n" j k grid.(j) grid.(k) a
            done;
        done;
    done;
*)
(*
    (* chaque colonne *)
    for i = 0 to w - 1 do
        (* chaque case et case + 1 *)
        for j = i * w to (i + 1) * w - 1 do
            for k = j + 1 to (i + 1) * w - 1 do
                let a = rows.(j) + rows.(k) in
                printf "%d %d - %4d %4d\n" i j rows.(j) rows.(k);
                if rows.(j) <> 0 && (a = 0 || abs a > 100) then printf "collision : [%d %d] (%d, %d) %d\n" j k rows.(j) rows.(k) a
            done;
        done;
    done;
*)
(*
    print_grid cols;
    print_grid rows
*)

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
    calc 0 0 + 2 * linear_conflict grid


(* ============================== *)


let get_neighbors grid (a, b as pt) =
    let moves = [
        (1, 0);
        (0, 1);
        (-1, 0);
        (0, -1);
    ]
    in
    List.fold_left (fun acc (x, y as mv) -> if is_move_valid grid pt mv then
        { cost = 0; prio = 0; grid = play_move grid pt mv; parent = grid;
            pos = (a + x, b + y); } :: acc else acc) []
         moves


(* TODO: rajouter tailles <> 3 *)
let is_solved grid =
    let grid' = [| 1; 2; 3; 8; 0; 4; 7; 6; 5 |]
    in
    grid = grid'


let for_each_neigh opened closed node neighbors =
    let rec aux opened closed = function
        | []        -> opened, closed
        | next :: t ->
                let new_cost = node.cost + 1 in
                if Pqueue.exists next.grid closed = false ||
                    new_cost < (Pqueue.get next.grid closed).cost then begin
                        let prio = new_cost + heuristic next.grid in
                        let closed = Pqueue.push node.grid { node with cost = new_cost } closed in
                        let opened = Pqueue.push next.grid
                            { next with cost = new_cost; prio = prio } opened in
                        aux opened closed t
                end
                else
                    aux opened closed t
    in
    aux opened closed neighbors

(*
                let new_cost = node.cost + 1 in
                if (Pqueue.exists next.grid opened &&
                    (Pqueue.get next.grid opened).cost < new_cost) ||
                    (Pqueue.exists next.grid closed &&
                    (Pqueue.get next.grid closed).cost < new_cost) then
                        aux opened closed t
                else begin
                    let prio = new_cost + heuristic next.grid in
                    let opened = Pqueue.push next.grid
                        { next with cost = new_cost; prio = prio } opened in
                    aux opened closed t
                end
*)


(* TODO: A enlever *)
exception Finished

let solved closed node =
    print_endline "Solved !";

    let rec get_answer parent =
        let node = Pqueue.get parent closed in
        if node.grid <> node.parent then begin
            get_answer node.parent;
            print_grid node.grid
        end
        else
            print_grid node.grid
    in
    get_answer node.grid;
    raise Finished


let solve grid =
    let start = {
        grid    = grid;
        parent  = grid;
        pos     = get_empty_case grid;
        prio    = 0;
        cost    = 0;
    }
    in
    let opened = Pqueue.push grid start Pqueue.empty
    and closed = Pqueue.create ()
    in

    let rec loop opened closed = 
        if Pqueue.is_empty opened then print_endline "Finished... no solutions"
        else begin
            let key, node, opened = Pqueue.pop_min opened in
            let neigh = get_neighbors node.grid node.pos in
(*             let closed = Pqueue.push key node closed in *)

            if is_solved node.grid then begin
                let closed = Pqueue.push key node closed in
                solved closed node
            end;

            let opened, closed = for_each_neigh opened closed node neigh
            in
            loop opened closed
        end
    in
    loop opened closed

let _ =
(*
    let grid = [| 3; 4; 2; 1; 6; 0; 8; 7; 5; |] in
    let grid = [| 1; 3; 2; 4; 6; 7; 0; 5; 8; |] in
    let grid = [| 2; 4; 8; 7; 0; 1; 3; 6; 5; |] in
    let grid = [| 0; 7; 8; 4; 5; 2; 3; 6; 1; |] in
    let grid = [| 7; 3; 1; 5; 8; 6; 0; 2; 4; |] in
    let grid = [| 3; 0; 1; 7; 2; 8; 5; 6; 4; |] in
    let grid = [| 0; 2; 6; 5; 7; 3; 4; 1; 8; |] in
    let grid = [| 6; 8; 4; 3; 2; 0; 5; 7; 1; |] in
    let grid = [| 4; 2; 7; 8; 1; 0; 5; 3; 6; |] in
*)
(*     let grid = [| 2; 4; 8; 7; 1; 5; 0; 3; 6; |] in *)
    (* Celui la est rapide *)
(*     let grid = [| 0; 4; 1; 8; 3; 2; 6; 7; 5; |] in  *)
(*     let grid = [| 1; 3; 0; 4; 6; 7; 8; 5; 2; |] in *)

(*     let grid = [| 8; 3; 4; 5; 7; 0; 2; 1; 6; |] in *)
(*     let grid = [| 2; 0; 4; 6; 8; 1; 5; 3; 7; |] in *)
(*     let grid = [| 0; 2; 8; 3; 6; 7; 5; 4; 1; |] in *)
(*     let grid = [| 4; 5; 1; 7; 0; 6; 3; 8; 2; |] in *)
(*     let grid = [| 6; 0; 5; 3; 2; 8; 4; 7; 1; |] in *)
(*     let grid = [| 6; 3; 4; 7; 8; 5; 1; 0; 2; |] in *)
(*     let grid = [| 1; 3; 5; 6; 7; 4; 8; 0; 2; |] in *)
(*     let grid = [| 4; 6; 8; 5; 2; 3; 0; 1; 7; |] in *)

(*     let grid = [| 0; 7; 4; 8; 6; 1; 5; 3; 2; |] in *)

(*     let grid = [| 7; 1; 5; 0; 3; 6; 8; 4; 2; |] in *)
(*     let grid = [| 3; 4; 1; 0; 2; 7; 6; 5; 8; |] in *)

    let grid = [| 5; 4; 2; 1; 3; 0; 7; 6; 8; |] in
(*     let grid = [| 4; 6; 1; 3; 2; 0; 7; 5; 8; |] in *)

(*
    let grid = [|
        1; 2; 3;
        4; 0; 5;
        6; 7; 8
        |]
    in
*)

(*
 * invalid
    let grid = [|
        4; 2; 5;
        1; 0; 6;
        3; 8; 7
        |]
    in
*)

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
        8; 7; 5;
        3; 0; 1;
        4; 2; 6
        |]
    in
*)
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
(* linear_conflict grid *)
    solve grid
