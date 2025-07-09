open! Core

type t = private {
  name: string;
  rank: Rank.t;
  modifiers: Modifier.Set.t;
  main_hero_pool: Hero.Set.t;
  secondary_hero_pool: Hero.Set.t;
  strength: int;
}
[@@deriving sexp, compare, hash]

val create : name:string -> Rank.t -> Modifier.t list -> Hero.t list -> Hero.t list -> t

val of_csv :
  name:string ->
  rank:string ->
  modifiers:string ->
  main_hero_pool:string ->
  secondary_hero_pool:string ->
  t

val to_string : t -> string

val strength : t -> int
