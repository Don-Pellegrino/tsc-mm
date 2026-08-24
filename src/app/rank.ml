open! Core

type t =
  | R1
  | R2 (* Ritualist *)
  | R3 (* Emissary *)
  | R4 (* Archon *)
  | R5 (* Oracle *)
  | R6 (* Phantom 1-3 *)
  | R7 (* Phantom 4-6 *)
  | R8 (* Ascendant 1-3 *)
  | R9 (* Ascendant 4-6 *)
  | R10 (* Eternus *)
[@@deriving sexp, hash]

let strength = function
| R1 -> 54
| R2 -> 64
| R3 -> 76
| R4 -> 88
| R5 -> 102
| R6 -> 116
| R7 -> 132
| R8 -> 152
| R9 -> 188
| R10 -> 240

let apply_multiplier rank multiplier =
  Float.(of_int (strength rank) * (multiplier - 1.0)) |> Float.round_nearest_half_to_even |> Float.to_int

let of_csv = function
| "1" -> R1
| "2" -> R2
| "3" -> R3
| "4" -> R4
| "5" -> R5
| "6" -> R6
| "7" -> R7
| "8" -> R8
| "9" -> R9
| "10" -> R10
| s -> failwithf "Invalid rank: %S" s ()

let is_low_rank = function
| R1
 |R2
 |R3 ->
  true
| R4
 |R5
 |R6
 |R7
 |R8
 |R9
 |R10 ->
  false

let is_high_rank = function
| R1
 |R2
 |R3
 |R4
 |R5
 |R6 ->
  true
| R7
 |R8
 |R9
 |R10 ->
  false
