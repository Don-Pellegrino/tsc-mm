open! Core

type t = private {
  players: Player.t list;
  main_hero_pool: Hero.Set.t;
  total_hero_pool: Hero.Set.t;
  main_hero_pool_size: int;
  total_hero_pool_size: int;
  strength: int;
}
[@@deriving sexp, compare, hash]

val create : Player.t list -> t

val to_string : show_strength:bool -> t -> string

val strength : t -> int

val random_player_strength_weighted : t -> Player.t

module Hero_players : sig
  type t0 = t

  type t = Player.t list Hero.Map.t

  val of_team : t0 -> t

  val combine : t list -> t

  val to_string : t -> string
end
