open! Core

module T = struct
  type t = {
    players: Player.t list;
    total_hero_pool: Hero.Set.t;
    total_hero_pool_size: int;
    strength: int;
  }
  [@@deriving sexp, compare, hash]
end

include T

let create players =
  let strengths = List.map players ~f:Player.strength in
  let max_strength, sum_strength =
    List.fold strengths ~init:(0, 0) ~f:(fun (acc_max, acc_sum) x -> max x acc_max, x + acc_sum)
  in
  let total_hero_pool =
    List.fold players ~init:Hero.Set.empty ~f:(fun acc p -> Set.union acc p.hero_pool)
  in
  let total_hero_pool_size = Set.length total_hero_pool in
  let strength = sum_strength + max_strength + (total_hero_pool_size * 2) in
  let players = List.sort ~compare:(fun p1 p2 -> [%compare: int] p2.strength p1.strength) players in
  { players; total_hero_pool; total_hero_pool_size; strength }

let to_string { players; _ } = List.map players ~f:Player.to_string |> String.concat ~sep:", "

let strength { strength; _ } = strength

let heroes { players; _ } =
  let init =
    List.fold players ~init:Hero.Map.empty ~f:(fun init p ->
      Set.fold p.hero_pool ~init ~f:(fun acc h -> Map.add_multi acc ~key:h ~data:p.name) )
  in
  List.fold Hero.all ~init ~f:(fun acc h ->
    match Map.add acc ~key:h ~data:[ "-" ] with
    | `Ok acc -> acc
    | `Duplicate -> acc )
