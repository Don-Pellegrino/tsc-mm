open! Core

module T = struct
  type t = {
    name: string;
    rank: Rank.t;
    modifiers: Modifier.Set.t;
    main_hero_pool: Hero.Set.t;
    secondary_hero_pool: Hero.Set.t;
    strength: int;
  }
  [@@deriving sexp, hash]

  let compare x y = [%compare: string] x.name y.name
end

include T

let create name rank modifiers main_hero_pool secondary_hero_pool =
  let modifiers = Modifier.Set.of_list modifiers in
  let main_hero_pool = Hero.Set.of_list main_hero_pool in
  let secondary_hero_pool = Set.diff (Hero.Set.of_list secondary_hero_pool) main_hero_pool in
  let strength =
    Rank.strength rank
    + (Set.length main_hero_pool / 2)
    + (Set.length secondary_hero_pool / 4)
    + Modifier.strength_total modifiers
  in
  { name; rank; modifiers; main_hero_pool; secondary_hero_pool; strength }

let parse_list raw parser =
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

let of_csv ~name ~rank ~modifiers ~main_hero_pool ~secondary_hero_pool =
  let rank = Rank.of_csv rank in
  let modifiers = parse_list modifiers Modifier.of_csv in
  let main_hero_pool = parse_list main_hero_pool Hero.of_csv in
  let secondary_hero_pool = parse_list secondary_hero_pool Hero.of_csv in
  create name rank modifiers main_hero_pool secondary_hero_pool

let to_string p = sprintf !"%s (%{sexp: Rank.t}, %d)" p.name p.rank p.strength

let strength { strength; _ } = strength

module Set = Set.Make (T)
