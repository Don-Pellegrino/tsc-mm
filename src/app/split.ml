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
  |> List.map ~f:(fun (h, ll) ->
       sprintf !"%{Hero}%s" h (List.map ll ~f:(sprintf "\t%s") |> String.concat) )
  |> String.concat ~sep:"\n"

let to_string (t1, t2) =
  sprintf
    !"Team 1 (%d): %s\n%{display_heroes}\n\nTeam 2 (%d): %s\n%{display_heroes}"
    (Team.strength t1) (Team.to_string t1) (Team.heroes t1) (Team.strength t2) (Team.to_string t2)
    (Team.heroes t2)

let imbalance (t1, t2) = Team.strength t1 - Team.strength t2 |> Int.abs

module Set = Set.Make (T)
module Table = Hashtbl.Make (T)
