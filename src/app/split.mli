open! Core

type t [@@deriving sexp, compare, hash]

module Table : Hashtbl.S with type key = t

val create : Team.t -> Team.t -> t

val teams : t -> [ `Amber of Team.t ] * [ `Sapphire of Team.t ]

val imbalance : t -> int

val strength_total : t -> int

type player_assignments = Hero.t Player.Map.t [@@deriving sexp, compare]

module Random_heroes : sig
  type t0 = t

  type t = {
    hero_by_player: player_assignments;
    points: (string * int) list;
    total_points: int;
    t1_frontliners: int;
    t2_frontliners: int;
  }
  [@@deriving sexp]

  type priorities =
    | Tertiary
    | Secondary

  val priorities_to_string : priorities -> string

  val generate : priorities -> t0 -> t
end
