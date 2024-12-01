open Base
open Stdio

let read_input () = "./input/day1.txt" |> In_channel.read_all
let print_list list = List.iter ~f:(Fn.compose print_endline Int.to_string) list

let parse_input input =
  input
  |> Helpers.split_on ~del:"\n"
  |> List.drop_last_exn
  |> List.map ~f:(fun str ->
    str |> Helpers.split_on ~del:"   " |> List.map ~f:Int.of_string)
  |> List.transpose_exn
;;

let part1_solve' input =
  let asc a b = if a < b then 1 else if a > b then -1 else 0 in
  let aList = List.nth_exn input 0 |> List.sort ~compare:asc in
  let bList = List.nth_exn input 1 |> List.sort ~compare:asc in

  let dif_list = List.map2_exn aList bList ~f:(fun a b -> Int.abs (a - b)) in

  List.fold ~init:0 ~f:( + ) dif_list
;;

let part1_solve () =
  read_input () |> parse_input |> part1_solve' |> Int.to_string |> print_endline
;;

let to_lookup_table list =
  let lookup = Map.empty (module Int) in

  let rec to_lookup_table' lookup = function
    | [] -> lookup
    | hd :: tl -> to_lookup_table' (Map.add_exn lookup ~key:hd ~data:0) tl
  in

  to_lookup_table' lookup list
;;

let part2_solve' input =
  let rec check_similarity list lookup =
    match list with
    | [] -> lookup
    | hd :: tl when Map.mem lookup hd ->
      check_similarity
        tl
        (Map.update lookup ~f:(fun i -> Option.value_exn i + 1) hd)
    | _ :: tl -> check_similarity tl lookup
  in

  let lookup =
    List.nth_exn input 0
    |> to_lookup_table
    |> check_similarity (List.nth_exn input 1)
  in

  Map.fold ~init:0 ~f:(fun ~key ~data acc -> acc + (key * data)) lookup
;;

let part2_solve () =
  read_input () |> parse_input |> part2_solve' |> Int.to_string |> print_endline
;;
