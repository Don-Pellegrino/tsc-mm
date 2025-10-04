open! Core

type strength = private {
  rank: int;
  main_hero_pool: int;
  secondary_hero_pool: int;
  difficulty_success: int;
  comms: int;
}
[@@deriving sexp]

type t = private {
  name: string;
  rank: Rank.t;
  difficulty: Modifier.Difficulty.t;
  success: Modifier.Success.t;
  comms: Modifier.Comms.Set.t;
  main_hero_pool: Hero.Set.t;
  secondary_hero_pool: Hero.Set.t;
  unselected_hero_pool: Hero.Set.t;
  strength_debug: (strength[@hash.ignore]);
  strength: int;
}
[@@deriving sexp, compare, hash]

val create :
  name:string ->
  Rank.t ->
  Modifier.Difficulty.t ->
  Modifier.Success.t ->
  Modifier.Comms.t list ->
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
