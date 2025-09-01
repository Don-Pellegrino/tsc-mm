open! Core

module Practicing = struct
  type t =
    | More
    | Usual
    | Less
    | Little
  [@@deriving sexp, hash]

  let of_csv = function
  | "More than usual!" -> More
  | "About the same as usual" -> Usual
  | "Less than usual" -> Less
  | "I basically haven't been playing Deadlock in the last few weeks" -> Little
  | s -> failwithf "Invalid Practicing modifier: %S" s ()

  let strength rank practicing =
    let multiplier =
      match practicing with
      | More -> 1.08
      | Usual -> 1.0
      | Less -> 0.9
      | Little -> 0.8
    in
    Rank.apply_multiplier rank multiplier
end

module Queueing = struct
  type t =
    | Solo
    | One_below
    | Two_below
    | One_above
    | Two_above
    | Same
    | Not_enough
  [@@deriving sexp, hash]

  let of_csv = function
  | "Solo queue 4 lyfe, baby" -> Solo
  | "Parties usually 1 rank BELOW mine. Example: Arcanist mostly playing with Alchemists" -> One_below
  | "Parties usually 2+ ranks below BELOW mine. Example: Arcanist mostly playing with Seekers" ->
    Two_below
  | "Parties usually 1 rank ABOVE mine. Example: Arcanist mostly playing with Ritualists" -> One_above
  | "Parties usually 2+ ranks above ABOVE mine. Example: Arcanist mostly playing with Emissary+" ->
    Two_above
  | "I pretty much only play with people the same rank as me." -> Same
  | "I don't play the game enough to know" -> Not_enough
  | s -> failwithf "Invalid Queueing modifier: %S" s ()

  let strength rank queueing =
    let multiplier =
      match queueing with
      | Solo -> 1.0
      | One_below -> 1.05
      | Two_below -> 1.1
      | One_above -> 1.03
      | Two_above -> 1.06
      | Same -> 1.03
      | Not_enough -> 0.95
    in
    Rank.apply_multiplier rank multiplier
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
      | Lead -> 1.1
      | Backup -> 1.05
      | Basic -> 1.02
      | Acknowledging -> 1.0
      | Locked_in -> 0.9
    in
    Rank.apply_multiplier rank multiplier
end
