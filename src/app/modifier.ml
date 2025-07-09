open! Core

module T = struct
  type t =
    | Ranking_up
    | Rank_stable
    | Ranking_down
    | Not_playing_as_much
    | Rank_too_high
    | Rank_accurate
    | Rank_too_low
    | Mainly_parties
    | Pressure_better
    | Pressure_worse
  [@@deriving sexp, compare, hash]
end

include T

let of_csv = function
| "In the last 2-3 weeks I've been (mostly or slowly) ranking up." -> Ranking_up
| "My rank in the last 2-3 weeks hasn't really changed." -> Rank_stable
| "In the last 2-3 weeks I've been (mostly or slowly) ranking down." -> Ranking_down
| "I haven't played as much recently." -> Not_playing_as_much
| "I feel like my current rank is too high." -> Rank_too_high
| "My current rank is probably accurate" -> Rank_accurate
| "I feel like my current rank is too low." -> Rank_too_low
| "I usually play in parties of 4, 5, or 6 people." -> Mainly_parties
| "I tend to perform better under pressure." -> Pressure_better
| "I tend to perform worse under pressure." -> Pressure_worse
| s -> failwithf "Invalid modifier: %S" s ()

let strength = function
| Ranking_up -> 2
| Rank_stable -> 0
| Ranking_down -> -2
| Not_playing_as_much -> -4
| Rank_too_high -> -5
| Rank_accurate -> 0
| Rank_too_low -> 5
| Mainly_parties -> -2
| Pressure_better -> 3
| Pressure_worse -> -3

let strength_total set = Set.fold set ~init:0 ~f:(fun acc x -> acc + strength x)

module Set = struct
  include Set.Make (T)
  include Provide_hash (T)
end
