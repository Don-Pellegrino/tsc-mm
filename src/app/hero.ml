open! Core

module T = struct
  type t =
    | Abrams
    | Bebop
    | Calico
    | Dynamo
    | Grey_Talon
    | Haze
    | Holliday
    | Infernus
    | Ivy
    | Kelvin
    | Lady_Geist
    | Lash
    | McGinnis
    | Mirage
    | Mo_Krill
    | Paradox
    | Pocket
    | Seven
    | Shiv
    | Sinclair
    | Vindicta
    | Viscous
    | Vyper
    | Warden
    | Wraith
    | Yamato
  [@@deriving sexp, compare, enumerate, hash]
end

include T

let to_string = function
| Abrams -> "Abrams"
| Bebop -> "Bebop"
| Calico -> "Calico"
| Dynamo -> "Dynamo"
| Grey_Talon -> "Grey Talon"
| Haze -> "Haze"
| Holliday -> "Holliday"
| Infernus -> "Infernus"
| Ivy -> "Ivy"
| Kelvin -> "Kelvin"
| Lady_Geist -> "Lady Geist"
| Lash -> "Lash"
| McGinnis -> "McGinnis"
| Mirage -> "Mirage"
| Mo_Krill -> "Mo & Krill"
| Paradox -> "Paradox"
| Pocket -> "Pocket"
| Seven -> "Seven"
| Shiv -> "Shiv"
| Sinclair -> "Sinclair"
| Vindicta -> "Vindicta"
| Viscous -> "Viscous"
| Vyper -> "Vyper"
| Warden -> "Warden"
| Wraith -> "Wraith"
| Yamato -> "Yamato"

module Set = struct
  include Set.Make (T)
  include Provide_hash (T)
end

module Map = Map.Make (T)
