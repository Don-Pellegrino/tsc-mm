open! Core

module Strength = struct
  type t = {
    player_strengths: Player.Strength.t list;
    total_player_strength: int;
    top_player_bonus: int;
    main_hero_pool_size_bonus: int;
    total_hero_pool_size_bonus: int;
  }
  [@@deriving sexp, compare]

  let total
    {
      player_strengths = _;
      total_player_strength;
      top_player_bonus;
      main_hero_pool_size_bonus;
      total_hero_pool_size_bonus;
    } =
    total_player_strength + top_player_bonus + main_hero_pool_size_bonus + total_hero_pool_size_bonus

  let to_string
    ( {
        player_strengths;
        total_player_strength;
        top_player_bonus;
        main_hero_pool_size_bonus;
        total_hero_pool_size_bonus;
      } as strength ) =
    let total_player_comms, total_player_pools =
      List.fold player_strengths ~init:(0, 0) ~f:(fun (acc_comms, acc_pools) ps ->
        acc_comms + ps.comms, acc_pools + ps.main_hero_pool + ps.secondary_hero_pool )
    in
    sprintf
      "- Total player strength: %d\n\
      \  - From comms: %d\n\
      \  - From hero pools: %d\n\
       - Top player bonus: %d\n\
       - Bonus for mains hero pool size: %d\n\
       - Bonus for draft strength: %d\n\
       TOTAL: **%d**" total_player_strength total_player_comms total_player_pools top_player_bonus
      main_hero_pool_size_bonus total_hero_pool_size_bonus (total strength)
end

module T = struct
  type t = {
    players: Player.t list;
    main_hero_pool: Hero.Set.t;
    total_hero_pool: Hero.Set.t;
    main_hero_pool_size: int;
    total_hero_pool_size: int;
    strength: (Strength.t[@hash.ignore]);
    total_strength: int;
  }
  [@@deriving sexp, compare, hash]
end

include T

let create (players : Player.t list) =
  let players =
    List.stable_sort players ~compare:(fun p1 p2 -> [%compare: int] p2.total_strength p1.total_strength)
  in
  let (top_player_strength, total_player_strength), player_strengths =
    List.fold_map players ~init:(0, 0) ~f:(fun (acc_max, acc_sum) { total_strength = x; strength; _ } ->
      (max acc_max x, acc_sum + x), strength )
  in
  let main_hero_pool, total_hero_pool =
    List.fold players ~init:(Hero.Set.empty, Hero.Set.empty) ~f:(fun (acc_main, acc_total) p ->
      ( Set.union acc_main p.main_hero_pool,
        Hero.Set.union_list [ acc_total; p.main_hero_pool; p.secondary_hero_pool ] ) )
  in
  let main_hero_pool_size = Set.length main_hero_pool in
  let total_hero_pool_size = Set.length total_hero_pool in
  let strength =
    Strength.
      {
        player_strengths;
        total_player_strength;
        top_player_bonus = top_player_strength / 2;
        main_hero_pool_size_bonus = Float.(of_int main_hero_pool_size * 1.5 |> to_int);
        total_hero_pool_size_bonus = total_hero_pool_size;
      }
  in
  let total_strength = Strength.total strength in
  {
    players;
    main_hero_pool;
    total_hero_pool;
    main_hero_pool_size;
    total_hero_pool_size;
    total_strength;
    strength;
  }

let has_player team player = List.mem ~equal:Player.equal team.players player

let to_string ~player_to_string ~shuffle_order { players; _ } =
  let players = Array.of_list players in
  if shuffle_order then Array.permute players;
  Array.map players ~f:(sprintf !"- %{player_to_string}\n") |> String.concat_array

let random_player_strength_weighted { players; _ } =
  let total_strength =
    List.fold players ~init:0 ~f:(fun acc { total_strength; _ } -> acc + total_strength)
  in
  let n = Random.int total_strength in
  List.fold_until players ~init:0
    ~finish:(fun x -> failwithf "Unexpected termination during random player selection at %d" x ())
    ~f:(fun acc ({ total_strength; _ } as p) ->
      if n < acc + total_strength then Stop p else Continue (acc + total_strength))

let player_position { players; _ } index = List.nth_exn players index

module Hero_players = struct
  type t0 = t

  type t = Player.t list Hero.Map.t

  let of_team { players; _ } =
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

  let combine : t list -> t = function
  | [] -> Hero.Map.empty
  | [ x ] -> x
  | init :: ll ->
    List.fold ll ~init ~f:(fun acc map ->
      Map.merge acc map ~f:(fun ~key -> function
        | `Left x
         |`Right x ->
          Some x
        | `Both (x, y) ->
          List.concat_no_order [ x; y ]
          |> List.sort ~compare:(fun p1 p2 ->
               (* Keep main heroes first *)
               match Set.mem p1.main_hero_pool key, Set.mem p2.main_hero_pool key with
               | true, true
                |false, false ->
                 0
               | true, false -> -1
               | false, true -> 1 )
          |> Option.return ) )

  let to_string (heroes : t) =
    Map.to_alist heroes
    |> List.map ~f:(fun (h, players) ->
         List.map players ~f:(fun (p : Player.t) ->
           sprintf "\t%s%s" (if Set.mem p.main_hero_pool h then "*" else "") p.name )
         |> String.concat
         |> sprintf !"%{Hero}%s" h )
    |> String.concat ~sep:"\n"
end
