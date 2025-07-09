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
| Seeker -> 0
| Alchemist -> 10
| Arcanist -> 20
| Ritualist -> 30
| Emissary -> 40
| Archon -> 50
| Oracle -> 60
