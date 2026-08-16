open! Core

module Strength : sig
  type t = private {
    rank: int;
    main_hero_pool: int;
    total_hero_pool: int;
    ranking_up_down: int;
    low_agency: int;
    comms: int;
  }
  [@@deriving sexp, compare]

  val total_strength : t -> int
end

type t = private {
  name: string;
  rank: Rank.t;
  ranking_up_down: Modifier.Ranking_up_down.t;
  comms: Modifier.Comms.Set.t;
  main_hero_pool: Hero.Set.t;
  secondary_hero_pool: Hero.Set.t;
  unselected_hero_pool: Hero.Set.t;
  strength: Strength.t;
  total_strength: int;
}
[@@deriving sexp, compare, equal, hash]

val create :
  name:string ->
  Rank.t ->
  Modifier.Ranking_up_down.t ->
  Modifier.Comms.t list ->
  Hero.t list ->
  Hero.t list ->
  t

val of_csv :
  name:string ->
  rank:string ->
  ranking_up_down:string ->
  comms:string ->
  main_hero_pool:string ->
  secondary_hero_pool:string ->
  t

val to_string : t -> string

module Map : Map.S with type Key.t = t
