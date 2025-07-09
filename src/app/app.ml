open! Core

let all_players =
  Import.of_filename "tsc2.csv"
  |> Array.fold ~init:String.Map.empty ~f:(fun acc player ->
       Map.add_exn acc ~key:player.name ~data:player )

let players =
  [
    "nip nop";
    (* "goop"; *)
    "Santiago";
    "_VITALITY_/Exodus";
    "Don Pellegrino";
    (* "Haversine"; *)
    "Wool";
    "Drubinda";
    "yeetorbyeetn";
    (* "Chard Czar"; *)
    "Agerikk";
    "sulster";
    "H.P. Schlubcraft";
    (* "pony"; *)
    "Jefe Yankee";
    (* "KrazeyOne"; *)
    "Lettuce goblin";
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
  let offset =
    match Sys.get_argv () with
    | [| _; s |] -> Int.of_string s
    | [||]
     |[| _ |] ->
      0
    | arr -> failwithf "Invalid arguments, expected 1, found: %d" (Array.length arr - 1) ()
  in
  let tbl = Split.Table.create () in
  generate [] 0 [] 0 tbl players;

  let splits =
    Hashtbl.to_alist tbl
    |> List.stable_sort ~compare:(fun (split1, imb1) (split2, imb2) ->
         match [%compare: int] imb1 imb2 with
         | 0 -> [%compare: int] (Split.strength_total split2) (Split.strength_total split1)
         | x -> x )
  in

  let split, imbalance = List.nth_exn splits offset in

  print_endline (sprintf !"%{sexp: Split.t}" split);
  print_endline (sprintf "Winning split (imbalance of %d):\n%s" imbalance (Split.to_string split))
