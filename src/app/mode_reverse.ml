open! Core

let run ~offset splits () =
  let split, imbalance = List.nth_exn splits offset in
  print_endline (sprintf !"%{sexp: Split.t}" split);

  let `Amber amber, `Sapphire sapphire = Split.teams split in

  let amber_player = Team.random_player_strength_weighted amber in
  let sapphire_player = Team.random_player_strength_weighted sapphire in

  (* TODO: flag to pick the submode here *)
  (* let index = 0 in
     let amber_player = Team.player_position amber index in
     let sapphire_player = Team.player_position sapphire index in *)
  print_endline
    (sprintf
       !"Imbalance: %d\n\n\
         :raised_hand: Amber Hand Leader: **%s** [FIRST PICK]\n\
         :gem: Sapphire Flame Leader: **%s**\n\n\
         :raised_hand: Amber Hand:\n\
         %s\n\n\
         :gem: Sapphire Flame:\n\
         %s\n\n\
         %{Team.Hero_players}"
       imbalance amber_player.name sapphire_player.name
       (Team.to_string ~show_strength:false amber)
       (Team.to_string ~show_strength:false sapphire)
       Team.Hero_players.(combine [ of_team amber; of_team sapphire ]) )
