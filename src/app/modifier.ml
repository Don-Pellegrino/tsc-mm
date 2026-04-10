open! Core

module Difficulty = struct
  type t =
    | Higher
    | Same
    | Lower
    | Dont_know
  [@@deriving sexp, hash]

  let of_csv = function
  | "A lot of my matches are of a HIGHER rank than my own" -> Higher
  | "They tend to be of the same rank" -> Same
  | "A lot of my matches are of a LOWER rank than my own" -> Lower
  | "I don't know and I can't even try to guess" -> Dont_know
  | s -> failwithf "Invalid Difficulty modifier: %S" s ()
end

module Success = struct
  type t =
    | Winning
    | Half
    | Lose
    | Not_enough
  [@@deriving sexp, hash]

  let of_csv = function
  | "I've had more wins than losses lately (:" -> Winning
  | "About 50/50" -> Half
  | "I've had more losses than wins lately ):" -> Lose
  | "I basically haven't been playing Deadlock in the last few weeks" -> Not_enough
  | s -> failwithf "Invalid Success modifier: %S" s ()
end

module Difficulty_Success = struct
  let strength rank (difficulty : Difficulty.t) (success : Success.t) =
    let multiplier, flat =
      match difficulty, success with
      | Higher, Winning -> 1.1, 4
      | Higher, Half -> 1.1, 0
      | Higher, (Lose | Not_enough) -> 1.0, -2
      | Same, Winning -> 1.05, 1
      | Same, Half -> 1.0, 0
      | Same, Lose -> 0.95, -1
      | Same, Not_enough -> 0.9, 0
      | Lower, Winning -> 1.0, -2
      | Lower, Half -> 1.0, -1
      | Lower, Lose -> 0.9, -2
      | Lower, Not_enough -> 0.8, 0
      | Dont_know, Winning -> 1.0, 1
      | Dont_know, Half -> 0.95, 0
      | Dont_know, (Lose | Not_enough) -> 0.90, 0
    in
    flat + Rank.apply_multiplier rank multiplier
end

module Comms = struct
  module T = struct
    type t =
      | Macro
      | Picks
      | Alone
      | Ult
      | Comms
      | Quiet
      | Roam
      | Items
    [@@deriving sexp, compare, hash]

    let is_positive = function
    | Macro -> true
    | Picks -> true
    | Alone -> false
    | Ult -> true
    | Comms -> true
    | Quiet -> false
    | Roam -> true
    | Items -> true

    let need_one_on_team = function
    | Macro -> true
    | Picks -> true
    | Alone -> false
    | Ult -> false
    | Comms -> true
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
  | "I tell my team when it's time to run urn, push walkers, farm, take midboss, etc." -> Macro
  | "I often help generate a lot of ganks and picks for my team" -> Picks
  | "I get caught and die alone at least 2-3 times per game" -> Alone
  | "When I have a big team ult I use it as much as possible, even on single targets sometimes" -> Ult
  | "I use my microphone more than most people, I keep the team's spirits up!" -> Comms
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
          | Macro -> 1.04, 2
          | Picks -> 1.04, 3
          | Alone -> 0.94, -5
          | Ult -> 1.06, 0
          | Comms -> 1.06, 2
          | Quiet -> 0.98, -5
          | Roam -> 1.06, 0
          | Items -> 1.06, 0
        in
        Float.(acc_multiplier + (multiplier - 1.0)), acc_flat + flat )
    in
    flat + Rank.apply_multiplier rank multiplier
end
