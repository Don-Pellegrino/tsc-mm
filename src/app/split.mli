open! Core

type t [@@deriving sexp, compare, hash]

module Table : Hashtbl.S with type key = t

val create : Team.t -> Team.t -> t

val teams : t -> [ `Amber of Team.t ] * [ `Sapphire of Team.t ]

val imbalance : t -> int

val strength_total : t -> int

type player_assignments = Hero.t Player.Map.t [@@deriving sexp, compare]

type random_heroes = {
  hero_by_player: player_assignments;
  points: (string * int) list;
  total_points: int;
  t1_frontliners: int;
  t2_frontliners: int;
}
[@@deriving sexp]

val random_heroes : t -> random_heroes
