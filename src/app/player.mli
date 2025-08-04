open! Core

type t = private {
  name: string;
  rank: Rank.t;
  practicing: Modifier.Practicing.t;
  queueing: Modifier.Queueing.t;
  comms: Modifier.Comms.t;
  main_hero_pool: Hero.Set.t;
  secondary_hero_pool: Hero.Set.t;
  strength: int;
}
[@@deriving sexp, compare, hash]

val create :
  name:string ->
  Rank.t ->
  Modifier.Practicing.t ->
  Modifier.Queueing.t ->
  Modifier.Comms.t ->
  Hero.t list ->
  Hero.t list ->
  t

val of_csv :
  name:string ->
  rank:string ->
  practicing:string ->
  queueing:string ->
  comms:string ->
  main_hero_pool:string ->
  secondary_hero_pool:string ->
  t

val to_string : t -> string

val strength : t -> int
