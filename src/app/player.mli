open! Core

type t = private {
  name: string;
  rank: Rank.t;
  difficulty: Modifier.Difficulty.t;
  success: Modifier.Success.t;
  comms: Modifier.Comms.t;
  main_hero_pool: Hero.Set.t;
  secondary_hero_pool: Hero.Set.t;
  unselected_hero_pool: Hero.Set.t;
  strength: int;
}
[@@deriving sexp, compare, hash]

val create :
  name:string ->
  Rank.t ->
  Modifier.Difficulty.t ->
  Modifier.Success.t ->
  Modifier.Comms.t ->
  Hero.t list ->
  Hero.t list ->
  t

val of_csv :
  name:string ->
  rank:string ->
  difficulty:string ->
  success:string ->
  comms:string ->
  main_hero_pool:string ->
  secondary_hero_pool:string ->
  t

val to_string : t -> string

val strength : t -> int

module Map : Map.S with type Key.t = t
