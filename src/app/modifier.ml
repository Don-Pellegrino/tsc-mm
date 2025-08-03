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
  | More -> 7
  | Usual -> 0
  | Less -> -7
  | Little -> -13
end

module Queueing = struct
  type t =
    | Solo
    | Small_highest
    | Small
    | Large_highest
    | Large
    | Other
  [@@deriving sexp, hash]

  let of_csv = function
  | "Solo queue 4 lyfe, baby" -> Solo
  | "Small parties (2-3), I'm usually the highest ranked in the party" -> Small_highest
  | "Small parties (2-3), I'm rarely the highest ranked in the party" -> Small
  | "Large parties (4-6), I'm usually one of the highest ranked in the party" -> Large_highest
  | "Large parties (4-6), I'm almost never one of the highest ranked in the party" -> Large
  | "I don't know" -> Other
  | s -> failwithf "Invalid Queueing modifier: %S" s ()

  let strength = function
  | Solo -> -4
  | Small_highest -> 4
  | Small -> -2
  | Large_highest -> 8
  | Large -> -5
  | Other -> 0
end

module Pressure = struct
  type t =
    | Better
    | Worse
    | Same
    | Dont_know
  [@@deriving sexp, hash]

  let of_csv = function
  | "Better" -> Better
  | "Worse" -> Worse
  | "About the same, it doesn't affect me" -> Same
  | "I don't know" -> Dont_know
  | s -> failwithf "Invalid Pressure modifier: %S" s ()

  let strength = function
  | Better -> 3
  | Worse -> -4
  | Same -> 0
  | Dont_know -> 0
end
