open! Core

type submode =
  | Random
  | Position of int

let run ~offset submode splits () =
  let split, imbalance = List.nth_exn splits offset in
  print_endline (sprintf !"Number of splits: %d\n%{sexp: Split.t}" (List.length splits) split);

  let `Amber amber, `Sapphire sapphire = Split.teams split in

  let amber_player, sapphire_player =
    match submode with
    | Random -> Team.random_player_strength_weighted amber, Team.random_player_strength_weighted sapphire
    | Position index -> Team.player_position amber index, Team.player_position sapphire index
  in

  print_endline
    (sprintf
       !"Imbalance: %d\n\n\
         :raised_hand: Amber Hand Leader: **%s** [FIRST PICK]\n\
         :gem: Sapphire Flame Leader: **%s**\n\n\
         :raised_hand: Amber Hand:\n\
         %s\n\
         :gem: Sapphire Flame:\n\
         %s\n\
         :raised_hand: Amber Hand:\n\
         %{Team.Strength}\n\n\
         :gem: Sapphire Flame:\n\
         %{Team.Strength}\n\n\
         %{Team.Hero_players}"
       imbalance amber_player.name sapphire_player.name
       (Team.to_string ~show_strength:false amber)
       (Team.to_string ~show_strength:false sapphire)
       amber.strength sapphire.strength
       Team.Hero_players.(combine [ of_team amber; of_team sapphire ]) )
