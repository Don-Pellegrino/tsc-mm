open! Core

let max_imbalance = 5

let max_points = 2

module Dedupe = Map.Make (struct
  type t = Split.player_assignments [@@deriving sexp, compare]
end)

let run players priorities splits () =
  print_endline (sprintf !"%{sexp: Player.t list}" players);
  let acceptable_splits =
    List.filter_map splits ~f:(fun (split, imbalance) ->
      Option.some_if (imbalance <= max_imbalance) split )
  in
  let splits =
    List.fold acceptable_splits ~init:Dedupe.empty ~f:(fun acc split ->
      Fn.apply_n_times ~n:100
        (fun acc ->
          let random = Split.Random_heroes.generate priorities split in
          if random.total_points <= max_points
          then Map.set acc ~key:random.hero_by_player ~data:(split, random)
          else acc)
        acc )
  in

  let () =
    (* Inspect a specific player *)
    let acc = Hero.Table.create () in
    let target = List.nth_exn players 0 in
    print_endline Player.(sprintf "Inspecting %s" target.name);
    Map.iter splits ~f:(fun (_, { hero_by_player; _ }) ->
      Hashtbl.incr acc (Map.find_exn hero_by_player target) );
    print_endline (sprintf !"%{sexp: int Hero.Table.t}\n" acc)
  in

  let num_splits = Map.length splits in
  let split, Split.Random_heroes.{ hero_by_player; points; total_points; _ } =
    Map.nth_exn splits (Random.int num_splits) |> snd
  in
  let `Amber amber, `Sapphire sapphire = Split.teams split in
  let display (players : Player.t list) =
    List.map players ~f:(fun p -> sprintf !"- %s (%{Hero})" p.name (Map.find_exn hero_by_player p))
    |> String.concat ~sep:"\n"
  in
  print_endline
    (sprintf
       !"Splits imbalance <=%d: %d\n\
         Combinations points <=%d: %d\n\
         Points (%d): %{sexp: (string * int) list}\n\n\
         :raised_hand: Amber Hand:\n\
         %s\n\n\
         :gem: Sapphire Flame:\n\
         %s\n"
       max_imbalance (List.length acceptable_splits) max_points num_splits total_points points
       (display amber.players) (display sapphire.players) )
