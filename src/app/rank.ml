open! Core

type t =
  | Seeker
  | Alchemist
  | Arcanist
  | Ritualist
  | Emissary
  | Archon
  | Oracle
[@@deriving sexp, hash]

let strength = function
| Seeker -> 50
| Alchemist -> 58
| Arcanist -> 67
| Ritualist -> 77
| Emissary -> 89
| Archon -> 102
| Oracle -> 117

let of_csv = function
| "Seeker (dark red)" -> Seeker
| "Alchemist (blue)" -> Alchemist
| "Arcanist (green)" -> Arcanist
| "Ritualist (orange)" -> Ritualist
| "Emissary (light red)" -> Emissary
| "Archon (purple)" -> Archon
| "Oracle (goat)" -> Oracle
| s -> failwithf "Invalid rank: %S" s ()
