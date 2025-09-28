open! Core

type t =
  | Initiate
  | Seeker
  | Alchemist
  | Arcanist
  | Ritualist
  | Emissary
  | Archon
  | Oracle
  | Phantom
[@@deriving sexp, hash]

let strength = function
| Initiate -> 43
| Seeker -> 50
| Alchemist -> 58
| Arcanist -> 67
| Ritualist -> 77
| Emissary -> 88
| Archon -> 100
| Oracle -> 113
| Phantom -> 127

let apply_multiplier rank multiplier =
  Float.(of_int (strength rank) * (multiplier - 1.0)) |> Float.round_nearest_half_to_even |> Float.to_int

let of_csv = function
| "Initiate (beige)" -> Initiate
| "Seeker (dark red)" -> Seeker
| "Alchemist (blue)" -> Alchemist
| "Arcanist (green)" -> Arcanist
| "Ritualist (orange)" -> Ritualist
| "Emissary (light red)" -> Emissary
| "Archon (purple)" -> Archon
| "Oracle (goat)" -> Oracle
| "Phantom and up" -> Phantom
| s -> failwithf "Invalid rank: %S" s ()
