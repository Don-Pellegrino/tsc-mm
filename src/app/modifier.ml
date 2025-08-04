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

  let strength = function
  | More -> 5
  | Usual -> 0
  | Less -> -8
  | Little -> -13
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

  let strength = function
  | Solo -> 0
  | One_below -> 3
  | Two_below -> 8
  | One_above -> 2
  | Two_above -> 6
  | Same -> 2
  | Not_enough -> -3
end

module Comms = struct
  type t =
    | Lead
    | Backup
    | Basic
    | Acknowledging
    | Listen_solo
    | Locked_in
  [@@deriving sexp, hash]

  let of_csv = function
  | "I tend to lead and make a lot of big macro calls" -> Lead
  | "I'll lead and make macro calls but only if no one else does" -> Backup
  | "I communicate but I rarely try to direct my team" -> Basic
  | "I'm mostly just listening and acknowledging" -> Acknowledging
  | "I listen but at the end of the day I mostly do my own thing TBH" -> Listen_solo
  | "I'm too locked in to speak. The team can play chess all they want, I'm just here for blood" ->
    Locked_in
  | s -> failwithf "Invalid Pressure modifier: %S" s ()

  let strength = function
  | Lead -> 4
  | Backup -> 3
  | Basic -> 2
  | Acknowledging -> 1
  | Listen_solo -> 0
  | Locked_in -> -3
end
