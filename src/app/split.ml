open! Core

module T = struct
  type t = Team.t * Team.t [@@deriving sexp, compare, hash]
end

include T

let create t1 t2 =
  match [%compare: Team.t] t1 t2 with
  | 1 -> t2, t1
  | _ -> t1, t2

let teams (t1, t2) =
  let t1_has_first_pick =
    let top2_even (team : Team.t) =
      match team.players with
      | [ p1; p2; _p3; _p4; _p5; _p6 ] -> p1.strength = p2.strength
      | _ -> failwithf !"Impossible case at %{Source_code_position}" [%here] ()
    in
    match top2_even t1, top2_even t2 with
    | true, true
     |false, false ->
      Random.bool ()
    | true, false -> false
    | false, true -> true
  in
  let amber, sapphire = if t1_has_first_pick then t1, t2 else t2, t1 in
  `Amber amber, `Sapphire sapphire

let imbalance (t1, t2) = Team.strength t1 - Team.strength t2 |> Int.abs

let strength_total (t1, t2) = Team.strength t1 + Team.strength t2

type player_assignments = Hero.t Player.Map.t [@@deriving sexp, compare]

type random_heroes = {
  hero_by_player: player_assignments;
  points: (string * int) list;
  total_points: int;
  t1_frontliners: int;
  t2_frontliners: int;
}
[@@deriving sexp]

let random_heroes ((t1, t2) : t) =
  let pool_diff pool acc = Set.filter pool ~f:(fun h -> Map.mem acc h |> not) in
  let prioritize (acc, points) player pools =
    List.find_map_exn pools ~f:(fun (pool, penalty) ->
      let available = pool_diff pool acc in
      if Set.is_empty available
      then None
      else (
        let points = if penalty = 0 then points else ("Pool", penalty) :: points in
        Some (Map.add_exn acc ~key:(Set.choose_exn available) ~data:player, points) ) )
  in

  let player_by_hero, points =
    let random_order () = 1 - Random.int 2 in
    List.concat_no_order [ t1.players; t2.players ]
    |> List.sort ~compare:(fun p1 p2 ->
         match p1.rank, p2.rank with
         | (Initiate | Alchemist), (Initiate | Alchemist) -> random_order ()
         | (Initiate | Alchemist), _ -> -1
         | _, (Initiate | Alchemist) -> 1
         | _ -> random_order () )
    |> List.fold ~init:(Hero.Map.empty, []) ~f:(fun acc -> function
         | { rank = Initiate | Alchemist; secondary_hero_pool; main_hero_pool; _ } as x ->
           prioritize acc x [ secondary_hero_pool, 0; main_hero_pool, 1; Hero.all_set, 10 ]
         | { unselected_hero_pool; secondary_hero_pool; main_hero_pool; _ } as x ->
           prioritize acc x [ unselected_hero_pool, 0; secondary_hero_pool, 1; main_hero_pool, 6 ] )
  in
  let hero_by_player =
    Map.fold player_by_hero ~init:Player.Map.empty ~f:(fun ~key ~data -> Map.add_exn ~key:data ~data:key)
  in
  let t1_frontliners, t2_frontliners =
    Tuple2.map (t1.players, t2.players)
      ~f:(List.count ~f:(fun p -> Map.find_exn hero_by_player p |> Hero.is_frontliner))
  in
  let points =
    let add_if cond x ll = if cond then x :: ll else ll in
    points
    |> add_if (t1_frontliners <> t2_frontliners)
         ("T1 more frontliners", t1_frontliners - t2_frontliners |> Int.abs)
    |> add_if (t1_frontliners = 0) ("T1 0 frontliners", 5)
    |> add_if (t1_frontliners = 1) ("T1 1 frontliner", 3)
    |> add_if (t2_frontliners = 0) ("T2 0 frontliners", 5)
    |> add_if (t2_frontliners = 1) ("T2 1 frontliner", 3)
  in
  let total_points = List.fold points ~init:0 ~f:(fun acc (_, x) -> acc + x) in
  { hero_by_player; points; total_points; t1_frontliners; t2_frontliners }

module Set = Set.Make (T)
module Table = Hashtbl.Make (T)
