open! Core

module Ranking_up_down = struct
  module T = struct
    type t =
      | Up_slowly
      | Up_quickly
      | Staying_same
      | Down_slowly
      | Down_quickly
      | Not_playing_much
    [@@deriving sexp, compare, hash]
  end

  include T

  let of_csv = function
  | "I've been slowly ranking up :)" -> Up_slowly
  | "I've been ranking up fairly quickly :D" -> Up_quickly
  | "I've been staying roughly the same" -> Staying_same
  | "I've been slowly ranking down :(" -> Down_slowly
  | "I've been ranking down fairly quickly D:" -> Down_quickly
  | "I don't know and I can't even try to guess because I haven't been playing much" -> Not_playing_much
  | s -> failwithf "Invalid Ranking_up_down modifier: %S" s ()

  let strength rank (ranking_up_down : t) =
    let multiplier =
      match ranking_up_down with
      | Up_slowly -> 1.1
      | Up_quickly -> 0.95 (* slow it down *)
      | Staying_same -> 1.0
      | Down_slowly -> 0.90
      | Down_quickly -> 1.05 (* slow it down *)
      | Not_playing_much -> 0.85
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
