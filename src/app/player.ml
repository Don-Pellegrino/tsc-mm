open! Core

module Strength = struct
  type t = {
    rank: int;
    main_hero_pool: int;
    secondary_hero_pool: int;
    difficulty_success: int;
    comms: int;
  }
  [@@deriving sexp, compare]

  let total_strength { rank; main_hero_pool; secondary_hero_pool; difficulty_success; comms } =
    rank + main_hero_pool + secondary_hero_pool + difficulty_success + comms
end

module T = struct
  type t = {
    name: string;
    rank: Rank.t;
    difficulty: Modifier.Difficulty.t;
    success: Modifier.Success.t;
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

let create ~name rank difficulty success comms main_hero_pool secondary_hero_pool =
  let main_hero_pool = Hero.Set.of_list main_hero_pool in
  let secondary_hero_pool = Set.diff (Hero.Set.of_list secondary_hero_pool) main_hero_pool in
  let unselected_hero_pool = Set.diff Hero.all_set (Set.union main_hero_pool secondary_hero_pool) in
  let comms = Modifier.Comms.Set.of_list comms in
  let strength =
    Strength.
      {
        rank = Rank.strength rank;
        main_hero_pool =
          min 3 (Set.length main_hero_pool)
          |> Float.of_int
          |> Float.( * ) 0.02
          |> Float.( + ) 1.0
          |> Rank.apply_multiplier rank;
        secondary_hero_pool =
          min 5 (Set.length secondary_hero_pool)
          |> Float.of_int
          |> Float.( * ) 0.01
          |> Float.( + ) 1.0
          |> Rank.apply_multiplier rank;
        difficulty_success = Modifier.Difficulty_Success.strength rank difficulty success;
        comms = Modifier.Comms.strength rank comms;
      }
  in
  let total_strength = Strength.total_strength strength in
  {
    name;
    rank;
    difficulty;
    success;
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

let of_csv ~name ~rank ~difficulty ~success ~comms ~main_hero_pool ~secondary_hero_pool =
  let rank = Rank.of_csv rank in
  let difficulty = Modifier.Difficulty.of_csv difficulty in
  let success = Modifier.Success.of_csv success in
  let comms = parse_list Modifier.Comms.of_csv comms in
  let main_hero_pool = parse_list Hero.of_csv main_hero_pool in
  let secondary_hero_pool = parse_list Hero.of_csv secondary_hero_pool in
  create ~name rank difficulty success comms main_hero_pool secondary_hero_pool

let to_string p = sprintf !"%s (%{sexp: Rank.t}, %d)" p.name p.rank p.total_strength

module Map = Map.Make (T)
