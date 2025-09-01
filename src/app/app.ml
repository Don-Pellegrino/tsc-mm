open! Core

let () = Random.self_init ()

let all_players =
  Import.of_filename "tsc.csv"
  |> Array.fold ~init:String.Map.empty ~f:(fun acc player ->
       Map.add_exn acc ~key:player.name ~data:player )

let players =
  [
    (* "Drubinda"; *)
    "Don Pellegrino";
    "nipnop";
    "yeetorbyeetn";
    "Shwabba Frog";
    "Chris!";
    "BARD";
    "Santiago";
    (* "Flockthebird"; *)
    (* "Swinton"; *)
    "tsu";
    "Jarf";
    "Narsty";
    "VentiAyahuascaBigGulp";
    "DrPocketz/Gwizz";
  ]
  |> List.map ~f:(Map.find_exn all_players)

let () =
  let len = List.length players in
  if len <> 12 then failwithf "Expected 12 players in the list, but found %d" len ()

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

  Hashtbl.to_alist tbl
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
        let%map () = Param.return () in
        Mode_random.run players priorities splits
      in
      let summary =
        sprintf
          !"Attempts to give each player a %s pick"
          (Split.Random_heroes.priorities_to_string priorities)
      in
      basic ~summary random
    in
    let secondary = random Secondary in
    let tertiary = random Tertiary in
    group ~summary:"Teams and heroes are assigned automatically" ~preserve_subcommand_order:()
      [ "secondary", secondary; "tertiary", tertiary ]
  in
  group ~summary:"The Scrap Circuit's matchmaker" ~preserve_subcommand_order:()
    [
      "standard", basic ~summary:"Standard draft mode" standard;
      "reverse", basic ~summary:"2 players draft heroes, then teams are revealed" reverse;
      "random", random;
    ]
  |> Command_unix.run
