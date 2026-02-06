open! Core

type submode =
  | Random
  | Position of int

let run ~offset submode splits () =
  let split, imbalance = List.nth_exn splits offset in
  print_endline (sprintf !"Number of splits: %d\n%{sexp: Split.t}" (List.length splits) split);

  let `HiddenKing hk, `ArchMother am = Split.teams split in

  let hk_player, am_player =
    match submode with
    | Random -> Team.random_player_strength_weighted hk, Team.random_player_strength_weighted am
    | Position index -> Team.player_position hk index, Team.player_position am index
  in

  print_endline
    (sprintf
       !"Imbalance: %d\n\n\
         :icon_hiddenking: Hidden King Leader: **%s** [FIRST PICK]\n\
         :icon_archmother: Arch Mother Leader: **%s**\n\n\
         :icon_hiddenking: Hidden King:\n\
         %s\n\
         :icon_archmother: Arch Mother:\n\
         %s\n\
         :icon_hiddenking: Hidden King:\n\
         %{Team.Strength}\n\n\
         :icon_archmother: Arch Mother:\n\
         %{Team.Strength}\n\n\
         %{Team.Hero_players}"
       imbalance hk_player.name am_player.name
       (Team.to_string hk ~shuffle_order:true ~player_to_string:(fun p -> p.name))
       (Team.to_string am ~shuffle_order:true ~player_to_string:(fun p -> p.name))
       hk.strength am.strength
       Team.Hero_players.(combine [ of_team hk; of_team am ]) )
