open! Core

let players =
  Player.
    [
      create "Don Pellegrino" Emissary Emissary 0 [ Infernus; Ivy; Lady_Geist; Mirage; Wraith; Yamato ];
      create "H.P. Schlubcraft" Emissary Emissary 0 [ Abrams; McGinnis; Wraith ];
      create "Peak" Ritualist Archon 2
        [
          Abrams;
          Bebop;
          Haze;
          Holliday;
          Infernus;
          Ivy;
          Lash;
          Mo_Krill;
          Paradox;
          Shiv;
          Sinclair;
          Vindicta;
          Vyper;
          Warden;
          Wraith;
        ];
      create "Lettuce goblin" Arcanist Emissary 1 [ Grey_Talon; Seven; Vindicta; Wraith ];
      create "tsukieru" Archon Oracle 2 [ Calico; Dynamo; McGinnis; Vyper; Warden ];
      create "KrazeyMan" Seeker Alchemist 4 [ Abrams; McGinnis ];
      create "_VITALITY_" Ritualist Ritualist 0
        [
          Abrams;
          Bebop;
          Dynamo;
          Grey_Talon;
          Haze;
          Holliday;
          Infernus;
          Ivy;
          Kelvin;
          Lady_Geist;
          Lash;
          Mirage;
          Mo_Krill;
          Paradox;
          Seven;
          Shiv;
          Viscous;
          Vyper;
          Warden;
          Wraith;
        ];
      create "~ Swinton" Alchemist Arcanist 3
        [ Abrams; Calico; Grey_Talon; Kelvin; McGinnis; Mo_Krill; Viscous ];
      create "yeetorbyeetn" Arcanist Arcanist 5
        [ Haze; Lady_Geist; Lash; Mirage; Mo_Krill; Paradox; Seven; Wraith ];
      create "sulster" Ritualist Emissary 2 [ Lady_Geist; Seven; Vindicta ];
      create "Deo" Archon Oracle 1
        [ Abrams; Haze; Infernus; Lash; Paradox; Shiv; Viscous; Warden; Wraith; Yamato ];
      create "Atlas" Archon Oracle 2 [ Infernus; Ivy; Paradox; Seven; Wraith ];
      (* create "izzy" Archon Archon 0
         [
           Abrams;
           Calico;
           Haze;
           Holliday;
           Lash;
           Mirage;
           Paradox;
           Seven;
           Shiv;
           Sinclair;
           Vindicta;
           Warden;
           Wraith;
           Yamato;
         ]; *)
      (* create "ZachShark1" Archon Oracle 1 [ Abrams; Ivy; Lash; Mo_Krill; Paradox; Shiv ]; *)
      (* create "Chard Czar" Ritualist Archon 3 [ Grey_Talon; Haze; Paradox; Pocket; Wraith ]; *)
    ]

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
  let tbl = Split.Table.create () in
  generate [] 0 [] 0 tbl players;
  let split, imbalance =
    Hashtbl.fold tbl ~init:(Hashtbl.choose_exn tbl) ~f:(fun ~key:split1 ~data:imb1 (split2, imb2) ->
      if imb1 < imb2 then split1, imb1 else split2, imb2 )
  in

  print_endline (sprintf !"%{sexp: Split.t}" split);
  print_endline (sprintf "Winning split (imbalance of %d):\n%s" imbalance (Split.to_string split))
