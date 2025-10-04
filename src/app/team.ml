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
    List.fold players ~init:(0, 0) ~f:(fun (acc_max, acc_sum) p ->
      let strength = Player.strength p in
      max strength acc_max, strength + acc_sum )
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

let has_player team player = List.mem ~equal:Player.equal team.players player

let to_string ~show_strength = function
| { players; _ } when show_strength -> List.map players ~f:Player.to_string |> String.concat ~sep:", "
| { players; _ } ->
  let players = Array.of_list players in
  Array.permute players;
  Array.map players ~f:(fun p -> sprintf "- %s\n" p.name) |> String.concat_array

let strength { strength; _ } = strength

let random_player_strength_weighted { players; _ } =
  let total_strength = List.fold players ~init:0 ~f:(fun acc { strength; _ } -> acc + strength) in
  let n = Random.int total_strength in
  List.fold_until players ~init:0
    ~finish:(fun x -> failwithf "Unexpected termination during random player selection at %d" x ())
    ~f:(fun acc ({ strength; _ } as p) ->
      if n < acc + strength then Stop p else Continue (acc + strength))

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
