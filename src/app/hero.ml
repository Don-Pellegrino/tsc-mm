open! Core

module T = struct
  type t =
    | Abrams
    | Bebop
    | Billy
    | Calico
    | Doorman
    | Drifter
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
    | Mina
    | Mirage
    | Mo_Krill
    | Paige
    | Paradox
    | Pocket
    | Seven
    | Shiv
    | Sinclair
    | Victor
    | Vindicta
    | Viscous
    | Vyper
    | Warden
    | Wraith
    | Yamato
  [@@deriving sexp, compare, enumerate, hash]
end

include T

let of_csv = function
| "Abrams" -> Abrams
| "Bebop" -> Bebop
| "Billy" -> Billy
| "Calico" -> Calico
| "Doorman" -> Doorman
| "Drifter" -> Drifter
| "Dynamo" -> Dynamo
| "Grey Talon" -> Grey_Talon
| "Haze" -> Haze
| "Holliday" -> Holliday
| "Infernus" -> Infernus
| "Ivy" -> Ivy
| "Kelvin" -> Kelvin
| "Lady Geist" -> Lady_Geist
| "Lash" -> Lash
| "McGinnis" -> McGinnis
| "Mina" -> Mina
| "Mirage" -> Mirage
| "Mo & Krill" -> Mo_Krill
| "Paige" -> Paige
| "Paradox" -> Paradox
| "Pocket" -> Pocket
| "Seven" -> Seven
| "Shiv" -> Shiv
| "Sinclair" -> Sinclair
| "Victor" -> Victor
| "Vindicta" -> Vindicta
| "Viscous" -> Viscous
| "Vyper" -> Vyper
| "Warden" -> Warden
| "Wraith" -> Wraith
| "Yamato" -> Yamato
| s -> failwithf "Invalid hero: %S" s ()

let to_string = function
| Abrams -> "Abrams"
| Bebop -> "Bebop"
| Billy -> "Billy"
| Calico -> "Calico"
| Doorman -> "Doorman"
| Drifter -> "Drifter"
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
| Mina -> "Mina"
| Mirage -> "Mirage"
| Mo_Krill -> "Mo & Krill"
| Paige -> "Paige"
| Paradox -> "Paradox"
| Pocket -> "Pocket"
| Seven -> "Seven"
| Shiv -> "Shiv"
| Sinclair -> "Sinclair"
| Victor -> "Victor"
| Vindicta -> "Vindicta"
| Viscous -> "Viscous"
| Vyper -> "Vyper"
| Warden -> "Warden"
| Wraith -> "Wraith"
| Yamato -> "Yamato"

let is_frontliner = function
| Abrams -> true
| Billy -> true
| Bebop -> true
| Calico -> true
| Doorman -> false
| Drifter -> true
| Dynamo -> false
| Grey_Talon -> false
| Haze -> false
| Holliday -> false
| Infernus -> false
| Ivy -> false
| Kelvin -> true
| Lady_Geist -> true
| Lash -> false
| McGinnis -> true
| Mina -> false
| Mirage -> false
| Mo_Krill -> true
| Paige -> false
| Paradox -> false
| Pocket -> false
| Seven -> false
| Shiv -> true
| Sinclair -> false
| Victor -> true
| Vindicta -> false
| Viscous -> false
| Vyper -> false
| Warden -> true
| Wraith -> false
| Yamato -> true

module Set = struct
  include Set.Make (T)
  include Provide_hash (T)
end

module Map = Map.Make (T)
module Table = Hashtbl.Make (T)

let all_set = Set.of_list all
