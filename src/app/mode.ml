open! Core

type t =
  | Standard
  | Reverse
  | Randomized

let of_string = function
| "standard"
 |"std" ->
  Standard
| "reverse" -> Reverse
| "random"
 |"randomized" ->
  Randomized
| s -> failwithf "Invalid game mode: %S" s ()
