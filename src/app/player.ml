open! Core

module Strength = struct
  type t = {
    rank: int;
    main_hero_pool: int;
    total_hero_pool: int;
    ranking_up_down: int;
    low_agency: int;
    comms: int;
  }
  [@@deriving sexp, compare]

  let total_strength { rank; main_hero_pool; total_hero_pool; ranking_up_down; low_agency; comms } =
    rank + main_hero_pool + total_hero_pool + ranking_up_down + low_agency + comms
end

module T = struct
  type t = {
    name: string;
    rank: Rank.t;
    ranking_up_down: Modifier.Ranking_up_down.t;
    comms: Modifier.Comms.Set.t;
    main_hero_pool: Hero.Set.t;
    secondary_hero_pool: Hero.Set.t;
    unselected_hero_pool: Hero.Set.t;
    strength: (Strength.t[@hash.ignore]);
    total_strength: int;
  }
  [@@deriving sexp, hash]

  let compare x y = [%compare: string] x.name y.name

  let equal x y = compare x y = 0
end

include T

let create ~name rank ranking_up_down comms main_hero_pool secondary_hero_pool =
  let main_hero_pool = Hero.Set.of_list main_hero_pool in
  let secondary_hero_pool = Set.diff (Hero.Set.of_list secondary_hero_pool) main_hero_pool in
  let total_hero_pool = Set.union main_hero_pool secondary_hero_pool in
  let unselected_hero_pool = Set.diff Hero.all_set total_hero_pool in
  let comms = Modifier.Comms.Set.of_list comms in
  let is_low_agency = Set.for_all main_hero_pool ~f:Hero.is_low_agency in
  let strength =
    Strength.
      {
        rank = Rank.strength rank;
        main_hero_pool =
          min 5 (Set.length main_hero_pool)
          |> Float.of_int
          |> Float.( * ) 0.012
          |> Float.( + ) 1.0
          |> Rank.apply_multiplier rank;
        total_hero_pool =
          min 10 (Set.length total_hero_pool)
          |> Float.of_int
          |> Float.( * ) 0.008
          |> Float.( + ) 1.0
          |> Rank.apply_multiplier rank;
        ranking_up_down = Modifier.Ranking_up_down.strength rank ranking_up_down;
        low_agency = (if is_low_agency then Rank.apply_multiplier rank 0.92 else 0);
        comms = Modifier.Comms.strength rank comms;
      }
  in
  let total_strength = Strength.total_strength strength in
  {
    name;
    rank;
    ranking_up_down;
    comms;
    main_hero_pool;
    secondary_hero_pool;
    unselected_hero_pool;
    strength;
    total_strength;
  }

let parse_list parser = function
| "" -> []
| raw ->
  let ll = String.split ~on:',' raw in
  let buf = Buffer.create 32 in
  let parsed =
    List.fold ~init:[] ll ~f:(fun acc s ->
      Buffer.add_string buf s;
      let reconst =
        let len = Buffer.length buf - 1 in
        if Char.(Buffer.nth buf 0 = ' ')
        then Buffer.To_string.sub buf ~pos:1 ~len
        else Buffer.contents buf
      in
      try
        let parsed = parser reconst in
        Buffer.clear buf;
        parsed :: acc
      with
      | _exn ->
        Buffer.add_char buf ',';
        acc )
  in
  if Buffer.length buf > 0 then (Buffer.contents buf |> parser) :: parsed else parsed

let of_csv ~name ~rank ~ranking_up_down ~comms ~main_hero_pool ~secondary_hero_pool =
  let rank = Rank.of_csv rank in
  let ranking_up_down = Modifier.Ranking_up_down.of_csv ranking_up_down in
  let comms = parse_list Modifier.Comms.of_csv comms in
  let main_hero_pool = parse_list Hero.of_csv main_hero_pool in
  let secondary_hero_pool = parse_list Hero.of_csv secondary_hero_pool in
  create ~name rank ranking_up_down comms main_hero_pool secondary_hero_pool

let to_string p = sprintf !"%s (%{sexp: Rank.t}, %d)" p.name p.rank p.total_strength

module Map = Map.Make (T)
