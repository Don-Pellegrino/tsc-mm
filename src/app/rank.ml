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
  | Phantom_low
  | Phantom_high
  | Ascendant_low
  | Ascendant_high
  | Eternus
[@@deriving sexp, hash]

let strength = function
| Initiate -> 30
| Seeker -> 37
| Alchemist -> 45
| Arcanist -> 54
| Ritualist -> 64
| Emissary -> 75
| Archon -> 87
| Oracle -> 100
| Phantom_low -> 107
| Phantom_high -> 114
| Ascendant_low -> 121
| Ascendant_high -> 128
| Eternus -> 142

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
| "Phantom 1-3" -> Phantom_low
| "Phantom 4-6" -> Phantom_high
| "Ascendant 1-3" -> Ascendant_low
| "Ascendant 4-6" -> Ascendant_high
| "Eternus" -> Eternus
| s -> failwithf "Invalid rank: %S" s ()

let is_low_rank = function
| Initiate
 |Seeker
 |Alchemist
 |Arcanist
 |Ritualist ->
  true
| Emissary
 |Archon
 |Oracle
 |Phantom_low
 |Phantom_high
 |Ascendant_low
 |Ascendant_high
 |Eternus ->
  false

let is_high_rank = function
| Initiate
 |Seeker
 |Alchemist
 |Arcanist
 |Ritualist
 |Emissary
 |Archon
 |Oracle
 |Phantom_low ->
  false
| Phantom_high
 |Ascendant_low
 |Ascendant_high
 |Eternus ->
  true
