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
| Initiate -> 40
| Seeker -> 50
| Alchemist -> 58
| Arcanist -> 67
| Ritualist -> 77
| Emissary -> 89
| Archon -> 102
| Oracle -> 117
| Phantom -> 134

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
