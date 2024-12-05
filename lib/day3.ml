open Base
open Stdio

let read_input () = "./input/day3.txt" |> In_channel.read_all

module Parser = struct
  type state =
    { index : int
    ; input : string
    }

  let make_state input = { index = 0; input }

  type 'a t = { run : state -> ('a * state) option }

  let return v = { run = (fun state -> Some (v, state)) }

  let bind v fn =
    { run =
        (fun state ->
          match v.run state with
          | None -> None
          | Some (v, state') -> (fn v).run state')
    }
  ;;
end

let char (c : char) =
  let run (state : Parser.state) =
    if String.length state.input - 1 < state.index
    then None
    else (
      let c' = String.get state.input state.index in
      if Char.equal c c'
      then Some (c', { state with index = state.index + 1 })
      else None)
  in

  Parser.{ run }
;;

let digit =
  let run (state : Parser.state) =
    if String.length state.input - 1 < state.index
    then None
    else (
      let c' = String.get state.input state.index in
      if Char.is_digit c'
      then Some (c', { state with index = state.index + 1 })
      else None)
  in

  Parser.{ run }
;;

let number =
  let run (state : Parser.state) =
    let acc = ref "" in
    let rec loop state =
      match digit.run state with
      | Some (result, state') ->
        acc := String.append !acc (String.of_char result);
        loop state'
      | None -> state
    in

    let state' = loop state in

    Some (Int.of_string !acc, state')
  in

  Parser.{ run }
;;

let ( let* ) = Parser.bind

let number_pair =
  let* _ = char 'm' in
  let* _ = char 'u' in
  let* _ = char 'l' in
  let* _ = char '(' in
  let* n1 = number in
  let* _ = char ',' in
  let* n2 = number in
  let* _ = char ')' in

  Parser.return (n1, n2)
;;

let part1_solve' input =
  let state = Parser.make_state input in

  let rec parse total (state : Parser.state) =
    if state.index > String.length state.input
    then total
    else (
      match number_pair.run state with
      | None -> parse total { state with index = state.index + 1 }
      | Some ((a, b), state') -> parse (total + (a * b)) state')
  in

  parse 0 state
;;

let part1_solve () =
  read_input () |> part1_solve' |> Int.to_string |> print_endline
;;

let ( <|> ) (a : 'a Parser.t) (b : 'a Parser.t) =
  let run state =
    match a.run state with
    | None -> b.run state
    | Some v -> Some v
  in

  Parser.{ run }
;;

type instruction =
  | Do
  | Dont
  | Pair of (int * int)

let parse_do =
  let* _ = char 'd' in
  let* _ = char 'o' in

  Parser.return Do
;;

let parse_dont =
  let* _ = char 'd' in
  let* _ = char 'o' in
  let* _ = char 'n' in
  let* _ = char '\'' in
  let* _ = char 't' in
  let* _ = char '(' in
  let* _ = char ')' in

  Parser.return Dont
;;

let parse_pair =
  let* v = number_pair in
  Parser.return (Pair v)
;;

let parse_instruction = parse_dont <|> parse_do <|> parse_pair

let part2_solve' input =
  let state = Parser.make_state input in

  let rec parse instructions (state : Parser.state) =
    if state.index > String.length state.input
    then instructions
    else (
      match parse_instruction.run state with
      | None -> parse instructions { state with index = state.index + 1 }
      | Some (i, state') -> parse (i :: instructions) state')
  in

  let rec sum enabled total instructions =
    match instructions, enabled with
    | [], _ -> total
    | Do :: tl, _ -> sum true total tl
    | Dont :: tl, _ -> sum false total tl
    | _ :: tl, false -> sum false total tl
    | Pair (a, b) :: tl, true -> sum true (total + (a * b)) tl
  in

  parse [] state |> List.rev |> sum true 0
;;

let part2_solve () =
  read_input () |> part2_solve' |> Int.to_string |> print_endline
;;
