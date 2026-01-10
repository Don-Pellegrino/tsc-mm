open! Core

let run ~offset ~items splits () =
  let split, imbalance = List.nth_exn splits offset in
  print_endline (sprintf !"Number of splits: %d\n%{sexp: Split.t}" (List.length splits) split);

  let `Amber amber, `Sapphire sapphire = Split.teams split in
  let player_to_string =
    let shuffled_items = ref (List.permute Items.T4.all) in
    let take_one () =
      match !shuffled_items with
      | x :: rest ->
        shuffled_items := rest;
        x
      | [] -> failwith "Impossible case: not enough T4 items"
    in
    if items
    then Player.((fun p -> sprintf !"%s (%{Items.T4})" p.name (take_one ())))
    else Player.((fun p -> p.name))
  in
  print_endline
    (sprintf
       !"Imbalance: %d\n\n\
         :raised_hand: Amber Hand [FIRST PICK]: \n\
         %s\n\
         :gem: Sapphire Flame: \n\
         %s\n\
         :raised_hand: Amber Hand:\n\
         %{Team.Strength}\n\n\
         :gem: Sapphire Flame:\n\
         %{Team.Strength}\n\n\
         Amber Hand (%d): \n\
         %s\n\
         %{Team.Hero_players}\n\n\
         Sapphire Flame (%d): \n\
         %s\n\
         %{Team.Hero_players}"
       imbalance
       (Team.to_string amber ~shuffle_order:true ~player_to_string)
       (Team.to_string sapphire ~shuffle_order:true ~player_to_string)
       amber.strength sapphire.strength amber.total_strength
       (Team.to_string amber ~shuffle_order:false ~player_to_string:Player.to_string)
       (Team.Hero_players.of_team amber) sapphire.total_strength
       (Team.to_string sapphire ~shuffle_order:false ~player_to_string:Player.to_string)
       (Team.Hero_players.of_team sapphire) )
