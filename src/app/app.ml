open! Core

let () = Random.self_init ()

let all_players =
  Import.of_filename "tsc.csv"
  |> Array.fold ~init:String.Map.empty ~f:(fun acc player ->
       Map.add_exn acc ~key:player.name ~data:player )

let players =
  [
    "Don Pellegrino";
    "Swinton";
    "yeetorbyeetn";
    "Milkaholic";
    "PO3M_34";
    "Ijustagirl";
    "nipnop";
    "Wool";
    "Browning";
    "Pony Soprano";
    "Drubinda";
    "tsu";
    (* "gema"; *)
  ]
  |> List.map ~f:(Map.find_exn all_players)

let enforce_pairings = [||] |> Array.map ~f:(Tuple2.map ~f:(Map.find_exn all_players))

let () =
  let len = List.length players in
  if len <> 12 then failwithf "Expected 12 players in the list, but found %d" len ();
  Array.iter enforce_pairings ~f:(fun (x, y) ->
    let equal = Player.equal in
    if not (List.mem players x ~equal && List.mem players y ~equal)
    then failwithf "Enforced pair not in active players: %s, %s" x.name y.name () )

let make_splits () =
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
  in
  let tbl = Split.Table.create () in
  generate [] 0 [] 0 tbl players;

  Hashtbl.filteri tbl ~f:(fun ~key:split ~data:_ ->
    Array.for_all enforce_pairings ~f:(Split.players_together split) )
  |> Hashtbl.to_alist
  |> List.stable_sort ~compare:(fun (_split1, imb1) (_split2, imb2) -> [%compare: int] imb1 imb2)

let () =
  let open Command in
  let open Command.Let_syntax in
  let splits = make_splits () in
  let offset =
    let%map offset =
      Param.(
        flag "-o" (optional_with_default 0 int) ~aliases:[ "--offset" ] ~full_flag_required:()
          ~doc:"OFFSET how splits to skip" )
    in
    if offset < 0 then failwithf "Offset must be >0 but it was %d" offset () else offset
  in
  let standard =
    let%map offset = offset in
    Mode_standard.run ~offset splits
  in
  let reverse =
    let%map offset = offset
    and submode =
      Param.(
        flag "-p" (optional int) ~aliases:[ "--position" ] ~full_flag_required:()
          ~doc:
            "POSITION Have the players of a specific position draft instead of a random weighted system" )
      >>| function
      | None -> Mode_reverse.Random
      | Some index -> Position index
    in
    Mode_reverse.run ~offset submode splits
  in
  let random =
    let random (priorities : Split.Random_heroes.priorities) =
      let random =
        let%map no_help_alchemists =
          Param.(
            flag "--no-help-alchemists" no_arg ~aliases:[] ~full_flag_required:()
              ~doc:"Do not give Alchemists and below a more familiar hero" )
        in
        let help_alchemists = not no_help_alchemists in
        Mode_random.run players priorities ~help_alchemists splits
      in
      let summary =
        sprintf
          !"Attempts to give each player a %s pick"
          (Split.Random_heroes.priorities_to_string priorities)
      in
      basic ~summary random
    in
    let primary = random Primary in
    let secondary = random Secondary in
    let tertiary = random Tertiary in
    group ~summary:"Teams and heroes are assigned automatically" ~preserve_subcommand_order:()
      [ "primary", primary; "secondary", secondary; "tertiary", tertiary ]
  in
  group ~summary:"The Scrap Circuit's matchmaker" ~preserve_subcommand_order:()
    [
      "standard", basic ~summary:"Standard draft mode" standard;
      "reverse", basic ~summary:"2 players draft heroes, then teams are revealed" reverse;
      "random", random;
    ]
  |> Command_unix.run
