open! Core

module T = struct
  type t = {
    players: Player.t list;
    main_hero_pool: Hero.Set.t;
    total_hero_pool: Hero.Set.t;
    main_hero_pool_size: int;
    total_hero_pool_size: int;
    strength: int;
  }
  [@@deriving sexp, compare, hash]
end

include T

let create players =
  let max_strength, sum_strength =
    let strengths = List.map players ~f:Player.strength in
    List.fold strengths ~init:(0, 0) ~f:(fun (acc_max, acc_sum) x -> max x acc_max, x + acc_sum)
  in
  let main_hero_pool, total_hero_pool =
    List.fold players ~init:(Hero.Set.empty, Hero.Set.empty) ~f:(fun (acc_main, acc_total) p ->
      ( Set.union acc_main p.main_hero_pool,
        Hero.Set.union_list [ acc_total; p.main_hero_pool; p.secondary_hero_pool ] ) )
  in
  let main_hero_pool_size = Set.length main_hero_pool in
  let total_hero_pool_size = Set.length total_hero_pool in

  let strength = (max_strength / 2) + sum_strength + main_hero_pool_size + (total_hero_pool_size / 2) in
  let players =
    List.stable_sort players ~compare:(fun p1 p2 -> [%compare: int] p2.strength p1.strength)
  in
  { players; main_hero_pool; total_hero_pool; main_hero_pool_size; total_hero_pool_size; strength }

let to_string ~show_strength = function
| { players; _ } when show_strength -> List.map players ~f:Player.to_string |> String.concat ~sep:", "
| { players; _ } ->
  let players = Array.of_list players in
  Array.permute players;
  Array.map players ~f:(fun p -> sprintf "- %s\n" p.name) |> String.concat_array

let strength { strength; _ } = strength

let hero_players { players; _ } =
  (* Doing it in 2 passes with secondary heroes first so the main heroes come out ahead *)
  let init =
    List.fold players ~init:Hero.Map.empty ~f:(fun init p ->
      Set.fold p.secondary_hero_pool ~init ~f:(fun acc h -> Map.add_multi acc ~key:h ~data:p) )
  in
  let init =
    List.fold players ~init ~f:(fun init p ->
      Set.fold p.main_hero_pool ~init ~f:(fun acc h -> Map.add_multi acc ~key:h ~data:p) )
  in
  List.fold Hero.all ~init ~f:(fun acc h ->
    match Map.add acc ~key:h ~data:[] with
    | `Ok acc -> acc
    | `Duplicate -> acc )
