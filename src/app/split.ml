open! Core

module T = struct
  type t = Team.t * Team.t [@@deriving sexp, compare, hash]
end

include T

let create t1 t2 =
  match [%compare: Team.t] t1 t2 with
  | 1 -> t2, t1
  | _ -> t1, t2

let teams t = t

let display_heroes heroes =
  Map.to_alist heroes
  |> List.map ~f:(fun (h, players) ->
       List.map players ~f:(fun (p : Player.t) ->
         sprintf "\t%s%s" (if Set.mem p.main_hero_pool h then "*" else "") p.name )
       |> String.concat
       |> sprintf !"%{Hero}%s" h )
  |> String.concat ~sep:"\n"

let to_string ((t1, t2) : t) =
  let t1_has_first_pick =
    let top2_even (team : Team.t) =
      match team.players with
      | [ p1; p2; _p3; _p4; _p5; _p6 ] -> p1.strength = p2.strength
      | _ -> failwithf !"Impossible case at %{Source_code_position}" [%here] ()
    in
    match top2_even t1, top2_even t2 with
    | true, true
     |false, false ->
      Random.bool ()
    | true, false -> false
    | false, true -> true
  in
  let amber, sapphire = if t1_has_first_pick then t1, t2 else t2, t1 in
  sprintf
    !"Amber Hand [FIRST PICK]: %s\n\
      Sapphire Flame: %s\n\n\
      Amber Hand (%d): %s\n\
      %{display_heroes}\n\n\
      Sapphire Flame (%d): %s\n\
      %{display_heroes}"
    (Team.to_string amber ~show_strength:false)
    (Team.to_string sapphire ~show_strength:false)
    (Team.strength amber)
    (Team.to_string amber ~show_strength:true)
    (Team.hero_players amber) (Team.strength sapphire)
    (Team.to_string sapphire ~show_strength:true)
    (Team.hero_players sapphire)

let imbalance (t1, t2) = Team.strength t1 - Team.strength t2 |> Int.abs

let strength_total (t1, t2) = Team.strength t1 + Team.strength t2

module Set = Set.Make (T)
module Table = Hashtbl.Make (T)
