open! Core

let run ~offset splits () =
  let split, imbalance = List.nth_exn splits offset in
  print_endline (sprintf !"%{sexp: Split.t}" split);

  let `Amber amber, `Sapphire sapphire = Split.teams split in
  print_endline
    (sprintf
       !"Imbalance: %d\n\n\
         :raised_hand: Amber Hand [FIRST PICK]: \n\
         %s\n\
         :gem: Sapphire Flame: \n\
         %s\n\
         Amber Hand (%d): %s\n\
         %{Team.Hero_players}\n\n\
         Sapphire Flame (%d): %s\n\
         %{Team.Hero_players}"
       imbalance
       (Team.to_string amber ~show_strength:false)
       (Team.to_string sapphire ~show_strength:false)
       (Team.strength amber)
       (Team.to_string amber ~show_strength:true)
       (Team.Hero_players.of_team amber) (Team.strength sapphire)
       (Team.to_string sapphire ~show_strength:true)
       (Team.Hero_players.of_team sapphire) )
