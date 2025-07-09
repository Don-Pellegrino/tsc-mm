open! Core

type t [@@deriving sexp, compare, hash]

val create : Player.t list -> t

val to_string : t -> string

val strength : t -> int

val heroes : t -> string list Hero.Map.t
