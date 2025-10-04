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
      | Help
      | Cooldowns
      | Items
      | Timers
      | Quiet
    [@@deriving sexp, compare, hash]
  end

  include T
  module CSet = Set

  module Set = struct
    include Set.Make (T)
    include Provide_hash (T)
  end

  let of_csv = function
  | "I discuss/make important macro calls for my team" -> Macro
  | "I call out potential ganks and picks" -> Picks
  | "I call for help when needed" -> Help
  | "I keep my team aware of my important cooldowns" -> Cooldowns
  | "I keep my team informed of the enemy's important items" -> Items
  | "I keep track of timers (buffs, midboss) for my team" -> Timers
  | "I don't use my microphone much during a match" -> Quiet
  | s -> failwithf "Invalid Comms modifier: %S" s ()

  let strength rank comms =
    let comms, quiet =
      CSet.partition_tf comms ~f:(function
        | Quiet -> false
        | _ -> true )
      |> Tuple2.map_snd ~f:(fun x -> CSet.is_empty x |> not)
    in
    let len = CSet.length comms |> Float.of_int |> Float.( * ) 0.01 in
    let multiplier = len |> Float.( * ) (if quiet then 0.5 else 2.0) |> Float.( + ) 1.0 in
    Rank.apply_multiplier rank multiplier
end
