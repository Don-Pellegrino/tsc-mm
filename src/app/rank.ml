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
| Alchemist -> 60
| Arcanist -> 70
| Ritualist -> 80
| Emissary -> 90
| Archon -> 100
| Oracle -> 110
