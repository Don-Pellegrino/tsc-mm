open! Core

let () = Random.self_init ()

let all_players =
  Import.of_filename "tsc.csv"
  |> Array.fold ~init:String.Map.empty ~f:(fun acc player ->
       Map.add_exn acc ~key:player.name ~data:player )

let players =
  [
    "Vitality";
    "Skullx";
    "Don Pellegrino";
    "~Swinton";
    "nip nop";
    "Santiago";
    "Shwabba  Frog";
    "ijustagurl";
    "Chris!";
    "Yeetorbyeetn";
    "Wyllace";
    (* "Madison (princess)"; *)
    "Punchbop > Beambop";
    (* "flockthebird"; *)
  ]
  |> List.map ~f:(Map.find_exn all_players)

let () =
  let len = List.length players in
  if len <> 12 then failwithf "Expected 12 players in the list, but found %d" len ()

let rec generate t1 n1 t2 n2 tbl = function
| _ when n1 > 6 || n2 > 6 -> ()
| [] -> (
  let split = Split.create (Team.create t1) (Team.create t2) in
  match Hashtbl.add tbl ~key:split ~data:(Split.imbalance split) with
  | `Duplicate -> ()
  | `Ok -> () )
| player :: rest ->
  generate (player :: t1) (n1 + 1) t2 n2 tbl rest;
  generate t1 n1 (player :: t2) (n2 + 1) tbl rest

let () =
  let mode, offset =
    match Sys.get_argv () with
    | [| _; mode; index |] -> Mode.of_string mode, Int.of_string index
    | [| _; mode |] -> Mode.of_string mode, 0
    | [||]
     |[| _ |] ->
      Standard, 0
    | arr -> failwithf "Invalid number of arguments, found: %d" (Array.length arr - 1) ()
  in
  let tbl = Split.Table.create () in
  generate [] 0 [] 0 tbl players;

  let splits =
    Hashtbl.to_alist tbl
    |> List.stable_sort ~compare:(fun (_split1, imb1) (_split2, imb2) -> [%compare: int] imb1 imb2)
  in

  let split, imbalance = List.nth_exn splits offset in

  print_endline
    (sprintf
       !"Generated %d splits\n%{sexp: Split.t}\n\nWinning split (imbalance of %d):\n"
       (Hashtbl.length tbl) split imbalance );
  match mode with
  | Standard ->
    let `Amber amber, `Sapphire sapphire = Split.teams split in
    print_endline
      (sprintf
         !":raised_hand: Amber Hand [FIRST PICK]: \n\
           %s\n\
           :gem: Sapphire Flame: \n\
           %s\n\
           Amber Hand (%d): %s\n\
           %{Team.Hero_players}\n\n\
           Sapphire Flame (%d): %s\n\
           %{Team.Hero_players}"
         (Team.to_string amber ~show_strength:false)
         (Team.to_string sapphire ~show_strength:false)
         (Team.strength amber)
         (Team.to_string amber ~show_strength:true)
         (Team.Hero_players.of_team amber) (Team.strength sapphire)
         (Team.to_string sapphire ~show_strength:true)
         (Team.Hero_players.of_team sapphire) )
  | Reverse ->
    let `Amber amber, `Sapphire sapphire = Split.teams split in
    print_endline
      (sprintf
         !":raised_hand: Amber Hand Leader: **%s** [FIRST PICK]\n\
           :gem: Sapphire Flame Leader: **%s**\n\n\
           :raised_hand: Amber Hand:\n\
           %s\n\n\
           :gem: Sapphire Flame:\n\
           %s\n\n\
           %{Team.Hero_players}"
         (Team.random_player_strength_weighted amber).name
         (Team.random_player_strength_weighted sapphire).name
         (Team.to_string ~show_strength:false amber)
         (Team.to_string ~show_strength:false sapphire)
         Team.Hero_players.(combine [ of_team amber; of_team sapphire ]) )
  | Randomized ->
    let acceptable_splits =
      List.filter_map splits ~f:(fun (split, imbalance) -> Option.some_if (imbalance < 6) split)
    in
    let splits =
      List.concat_map acceptable_splits ~f:(fun split ->
        List.init 100 ~f:(fun _i ->
          let random = Split.random_heroes split in
          split, random ) )
      |> List.dedup_and_sort ~compare:(fun (_, r1) (_, r2) ->
           [%compare: Split.player_assignments] r1.hero_by_player r2.hero_by_player )
      |> List.filter ~f:(fun (_, { total_points; _ }) -> total_points < 3)
    in

    let () =
      let acc = Hero.Table.create () in
      let target = List.nth_exn players 4 in
      print_endline target.name;
      List.iter splits ~f:(fun (_, { hero_by_player; _ }) ->
        Hashtbl.incr acc (Map.find_exn hero_by_player target) );
      print_endline (sprintf !"%{sexp: int Hero.Table.t}" acc)
    in

    let split, Split.{ hero_by_player; points; total_points; _ } = List.random_element_exn splits in
    let `Amber amber, `Sapphire sapphire = Split.teams split in
    let display (players : Player.t list) =
      List.map players ~f:(fun p -> sprintf !"- %s (%{Hero})" p.name (Map.find_exn hero_by_player p))
      |> String.concat ~sep:"\n"
    in
    print_endline
      (sprintf
         !"Splits imbalance<5: %d\n\
           Combinations points<3: %d\n\
           Points (%d): %{sexp: (string * int) list}\n\n\
           :raised_hand: Amber Hand:\n\
           %s\n\n\
           :gem: Sapphire Flame:\n\
           %s\n"
         (List.length acceptable_splits) (List.length splits) total_points points (display amber.players)
         (display sapphire.players) )
