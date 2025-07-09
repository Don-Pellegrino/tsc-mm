open! Core

module T = struct
  type t = {
    name: string;
    current_rank: Rank.t;
    highest_rank: Rank.t;
    highest_months_ago: int;
    hero_pool: Hero.Set.t;
    strength: int;
  }
  [@@deriving sexp, hash]

  let compare x y = [%compare: string] x.name y.name
end

include T

let create name current_rank highest_rank highest_months_ago hero_pool =
  let hero_pool = Hero.Set.of_list hero_pool in
  let strength = Rank.strength current_rank + ((Rank.strength highest_rank - highest_months_ago) / 2) in
  { name; current_rank; highest_rank; highest_months_ago; hero_pool; strength }

let to_string p = sprintf !"%s (%{sexp: Rank.t}, %d)" p.name p.current_rank p.strength

let strength { strength; _ } = strength

module Set = Set.Make (T)
