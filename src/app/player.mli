open! Core

type t = private {
  name: string;
  current_rank: Rank.t;
  highest_rank: Rank.t;
  highest_months_ago: int;
  hero_pool: Hero.Set.t;
  strength: int;
}
[@@deriving sexp, compare, hash]

val create : string -> Rank.t -> Rank.t -> int -> Hero.t list -> t

val to_string : t -> string

val strength : t -> int
