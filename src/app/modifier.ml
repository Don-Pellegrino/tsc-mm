open! Core

module Practice = struct
  module T = struct
    type t =
      | Live_and_breathe
      | More
      | Same
      | Less
      | Back_from_break
      | On_a_break
    [@@deriving sexp, compare, hash]
  end

  include T

  let of_csv = function
  | "I live and breathe Deadlock right now" -> Live_and_breathe
  | "More than usual" -> More
  | "About the same as usual" -> Same
  | "Less than usual" -> Less
  | "I'm just coming back from a break" -> Back_from_break
  | "I'm mostly taking a break right now" -> On_a_break
  | s -> failwithf "Invalid Practice modifier: %S" s ()

  let strength rank (practice : t) =
    let multiplier =
      match practice with
      | Live_and_breathe -> 1.06
      | More -> 1.03
      | Same -> 1.0
      | Less -> 0.96
      | Back_from_break -> 0.90
      | On_a_break -> 0.86
    in
    Rank.apply_multiplier rank multiplier
end

module Comms = struct
  module T = struct
    type t =
      | Draft
      | Macro
      | Picks
      | Alone
      | Ult
      | Morale
      | Quiet
      | Roam
      | Items
    [@@deriving sexp, compare, hash]

    let is_positive = function
    | Draft -> true
    | Macro -> true
    | Picks -> true
    | Alone -> false
    | Ult -> true
    | Morale -> true
    | Quiet -> false
    | Roam -> true
    | Items -> true

    let need_one_on_team = function
    | Draft -> true
    | Macro -> true
    | Picks -> true
    | Alone -> false
    | Ult -> false
    | Morale -> true
    | Quiet -> false
    | Roam -> true
    | Items -> true
  end

  include T
  module CSet = Set

  module Set = struct
    include Set.Make (T)
    include Provide_hash (T)
  end

  let of_csv = function
  | "I know how to draft a team comp: hero synergies, strong lane duos, weak lane duos, etc." -> Draft
  | "I tell my team when it's time to run urn, push walkers, farm, take midboss, etc." -> Macro
  | "I initiate many ganks and picks for my team" -> Picks
  | "I get caught and die alone at least 2-3 times per game" -> Alone
  | "When I have a big team ult I use it as much as possible, even on single targets sometimes" -> Ult
  | "I'm basically the team mascot, I'm the one encouraging the team when things are looking dire" ->
    Morale
  | "I don't use my microphone much during a match" -> Quiet
  | "I often spend more time on the enemy's side of the map than mine" -> Roam
  | "I always buy at least 1-2 items that utterly cripple specific enemy heroes (ex. Slowing Hex against \
     Calico, Crippling Headshots against Victor)" ->
    Items
  | s -> failwithf "Invalid Comms modifier: %S" s ()

  let strength rank comms =
    let multiplier, flat =
      CSet.fold comms ~init:(1.0, 0) ~f:(fun (acc_multiplier, acc_flat) comm ->
        let multiplier, flat =
          match comm with
          | Draft -> 1.02, 5
          | Macro -> 1.02, 5
          | Picks -> 1.02, 4
          | Alone -> 0.94, -5
          | Ult -> 1.02, 0
          | Morale -> 1.03, 2
          | Quiet -> 0.96, -2
          | Roam -> 1.03, 0
          | Items -> 1.02, 5
        in
        Float.(acc_multiplier + (multiplier - 1.0)), acc_flat + flat )
    in
    let bonus = flat + Rank.apply_multiplier rank multiplier in
    min bonus Float.(Rank.strength rank // 5 |> to_int)
end
