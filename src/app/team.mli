open! Core

module Strength : sig
  type t = private {
    player_strengths: Player.Strength.t list;
    total_player_strength: int;
    top_player_bonus: int;
    main_hero_pool_size_bonus: int;
    total_hero_pool_size_bonus: int;
  }
  [@@deriving sexp, compare]

  val total : t -> int

  val to_string : t -> string
end

type t = private {
  players: Player.t list;
  main_hero_pool: Hero.Set.t;
  total_hero_pool: Hero.Set.t;
  main_hero_pool_size: int;
  total_hero_pool_size: int;
  strength: Strength.t;
  total_strength: int;
}
[@@deriving sexp, compare, hash]

val create : Player.t list -> t

val has_player : t -> Player.t -> bool

val to_string : show_strength:bool -> t -> string

val random_player_strength_weighted : t -> Player.t

val player_position : t -> int -> Player.t

module Hero_players : sig
  type t0 = t

  type t = Player.t list Hero.Map.t

  val of_team : t0 -> t

  val combine : t list -> t

  val to_string : t -> string
end
