open Base
open Stdio

let read_input () = "./input/day2.txt" |> In_channel.read_all

let parse_input input =
  let to_int_list str =
    Helpers.split_on ~del:" " str |> List.map ~f:Int.of_string
  in

  input |> String.strip |> Helpers.split_on ~del:"\n" |> List.map ~f:to_int_list
;;

let min_delta = 1
let max_delta = 3

type state =
  | Inc
  | Dec
  | Undermined

let is_safe retry (level : int list)  =
  let out_of_range a b =
    not (Int.between ~low:min_delta ~high:max_delta (Int.abs (a - b)))
  in
  let rec is_safe'
    (level : int list)
    (state : state)
    (prev : int)
    (retry : bool)
    =
    match level, state, prev, retry with
    | [], _, _, _ -> true
    | hd :: _, _, prev, false when out_of_range hd prev -> false
    | hd :: tl, state, prev, true when out_of_range hd prev ->
      is_safe' tl state hd false
    | hd :: _, Dec, prev, false when hd - prev >= 0 -> false
    | hd :: tl, Dec, prev, true when hd - prev >= 0 ->
      is_safe' tl Dec hd false
    | hd :: _, Inc, prev, false when hd - prev <= 0 -> false
    | hd :: tl, Inc, prev, true when hd - prev <= 0 ->
      is_safe' tl Inc hd false
    | hd :: tl, Undermined, prev, retry ->
      let s = if hd - prev > 0 then Inc else Dec in
      is_safe' tl s hd retry
    | hd :: tl, state, _, retry -> is_safe' tl state hd retry
  in

  is_safe' (List.drop level 1) Undermined (List.hd_exn level) retry
;;

let part1_solve' input =
  input |> List.map ~f:(is_safe false) |> List.count ~f:(Bool.( = ) true)
;;

let part1_solve () =
  read_input () |> parse_input |> part1_solve' |> Int.to_string |> print_endline
;;


let part2_solve' input =
  input |> List.map ~f:(is_safe true) |> List.count ~f:(Bool.( = ) true)
;;

let part2_solve () =
  read_input () |> parse_input |> part2_solve' |> Int.to_string |> print_endline
;;
