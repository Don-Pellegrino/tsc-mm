open! Core

type t [@@deriving sexp, compare, hash]

module Table : Hashtbl.S with type key = t

val create : Team.t -> Team.t -> t

val teams : t -> Team.t * Team.t

val to_string : t -> string

val imbalance : t -> int
