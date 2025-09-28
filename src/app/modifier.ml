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
  type t =
    | Lead
    | Backup
    | Basic
    | Acknowledging
    | Locked_in
  [@@deriving sexp, hash]

  let of_csv = function
  | "I tend to lead and make a lot of big macro calls" -> Lead
  | "I'll lead and make macro calls but only if no one else does" -> Backup
  | "I communicate but I rarely try to direct my team" -> Basic
  | "I'm mostly just listening and acknowledging" -> Acknowledging
  | "I'm too locked in to speak. The team can play chess all they want, I'm just here for blood" ->
    Locked_in
  | s -> failwithf "Invalid Pressure modifier: %S" s ()

  let strength rank comms =
    let multiplier =
      match comms with
      | Lead -> 1.05
      | Backup -> 1.02
      | Basic -> 1.1
      | Acknowledging -> 1.0
      | Locked_in -> 0.9
    in
    Rank.apply_multiplier rank multiplier
end
