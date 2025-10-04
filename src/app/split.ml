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

module Random_heroes = struct
  type t0 = t

  type t = {
    hero_by_player: player_assignments;
    points: (string * int) list;
    total_points: int;
    t1_frontliners: int;
    t2_frontliners: int;
    t1_carries: int;
    t2_carries: int;
    t1_picks: int;
    t2_picks: int;
    t1_teamfighters: int;
    t2_teamfighters: int;
  }
  [@@deriving sexp]

  type priorities =
    (* TODO: add Primary *)
    (* TODO: add a CLI option to turn off Alchemist special treatment *)
    | Tertiary
    | Secondary

  let priorities_to_string = function
  | Tertiary -> "tertiary"
  | Secondary -> "secondary"

  type comp_stats = {
    mutable frontliners: int;
    mutable carries: int;
    mutable picks: int;
    mutable teamfighters: int;
  }

  let empty_stats () = { frontliners = 0; carries = 0; picks = 0; teamfighters = 0 }

  let generate priorities ((t1, t2) : t0) =
    let pool_diff pool acc = Set.filter pool ~f:(fun h -> Map.mem acc h |> not) in
    let prioritize (acc, points) (player : Player.t) pools =
      List.find_map_exn pools ~f:(fun (pool, penalty) ->
        let available = pool_diff pool acc in
        if Set.is_empty available
        then None
        else (
          let points =
            if penalty = 0
            then points
            else (sprintf "Pool[%s -%d]" player.name penalty, penalty) :: points
          in
          Some (Map.add_exn acc ~key:(Set.choose_exn available) ~data:player, points) ) )
    in

    let player_by_hero, points =
      let random_order () = 1 - Random.int 2 in
      let all_players = Array.append (Array.of_list t1.players) (Array.of_list t2.players) in
      Array.sort all_players ~compare:(fun p1 p2 ->
        match p1.rank, p2.rank with
        | (Initiate | Alchemist), (Initiate | Alchemist) -> random_order ()
        | (Initiate | Alchemist), _ -> -1
        | _, (Initiate | Alchemist) -> 1
        | _ -> random_order () );
      Array.fold all_players ~init:(Hero.Map.empty, []) ~f:(fun acc player ->
        let order =
          match priorities, player with
          (* | Tertiary, { rank = Initiate | Alchemist; secondary_hero_pool; main_hero_pool; _ } ->
             [ secondary_hero_pool, 0; main_hero_pool, 1; Hero.all_set, 10 ] *)
          | Tertiary, { unselected_hero_pool; secondary_hero_pool; main_hero_pool; _ } ->
            [ unselected_hero_pool, 0; secondary_hero_pool, 1; main_hero_pool, 6 ]
          (* | Secondary, { rank = Initiate | Alchemist; secondary_hero_pool; main_hero_pool; _ } ->
             [ main_hero_pool, 0; secondary_hero_pool, 1; Hero.all_set, 10 ] *)
          | Secondary, { unselected_hero_pool; secondary_hero_pool; main_hero_pool; _ } ->
            [ secondary_hero_pool, 0; unselected_hero_pool, 1; main_hero_pool, 6 ]
        in
        prioritize acc player order )
    in
    let hero_by_player =
      Map.fold player_by_hero ~init:Player.Map.empty ~f:(fun ~key ~data ->
        Map.add_exn ~key:data ~data:key )
    in
    let load (team : Team.t) =
      let stats = empty_stats () in
      List.iter team.players ~f:(fun p ->
        let hero = Map.find_exn hero_by_player p in
        if Hero.is_frontliner hero then stats.frontliners <- stats.frontliners + 1;
        if Hero.is_carry hero then stats.carries <- stats.carries + 1;
        if Hero.is_pick hero then stats.picks <- stats.picks + 1;
        if Hero.is_teamfighter hero then stats.teamfighters <- stats.teamfighters + 1 );
      stats
    in
    let t1_stats = load t1 in
    let t2_stats = load t2 in
    let points =
      let add_if cond x ll = if cond then x :: ll else ll in
      points
      |> add_if
           (t1_stats.frontliners <> t2_stats.frontliners)
           ("frontliners imbalance", t1_stats.frontliners - t2_stats.frontliners |> Int.abs)
      |> add_if (t1_stats.frontliners = 0) ("T1 0 frontliners", 5)
      |> add_if (t1_stats.frontliners = 1) ("T1 1 frontliner", 2)
      |> add_if (t2_stats.frontliners = 0) ("T2 0 frontliners", 5)
      |> add_if (t2_stats.frontliners = 1) ("T2 1 frontliner", 2)
      |> add_if (t1_stats.carries = 0) ("T1 0 carries", 3)
      |> add_if (t2_stats.carries = 0) ("T2 0 carries", 3)
      |> add_if (t1_stats.carries > 1) ("T1 2+ carries", t1_stats.carries)
      |> add_if (t2_stats.carries > 1) ("T2 2+ carries", t2_stats.carries)
      |> add_if (t1_stats.picks = 0) ("T1 0 picks", 1)
      |> add_if (t2_stats.picks = 0) ("T2 0 picks", 1)
      |> add_if (t1_stats.picks > 2) ("T1 2+ picks", t1_stats.picks)
      |> add_if (t2_stats.picks > 2) ("T2 2+ picks", t2_stats.picks)
      |> add_if (t1_stats.teamfighters = 0) ("T1 0 teamfighters", 1)
      |> add_if (t2_stats.teamfighters = 0) ("T2 0 teamfighters", 1)
      |> add_if (t1_stats.teamfighters > 2) ("T1 2+ teamfighters", t1_stats.teamfighters)
      |> add_if (t2_stats.teamfighters > 2) ("T2 2+ teamfighters", t2_stats.teamfighters)
    in

    let total_points = List.fold points ~init:0 ~f:(fun acc (_, x) -> acc + x) in
    {
      hero_by_player;
      points;
      total_points;
      t1_frontliners = t1_stats.frontliners;
      t2_frontliners = t2_stats.frontliners;
      t1_carries = t1_stats.carries;
      t2_carries = t2_stats.carries;
      t1_picks = t1_stats.picks;
      t2_picks = t2_stats.picks;
      t1_teamfighters = t1_stats.teamfighters;
      t2_teamfighters = t2_stats.teamfighters;
    }
end

module Table = Hashtbl.Make (T)
