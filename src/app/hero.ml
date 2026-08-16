open! Core

module T = struct
  type t =
    | Abrams
    | Apollo
    | Bebop
    | Billy
    | Calico
    | Celeste
    | Doorman
    | Drifter
    | Dynamo
    | Graves
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
    | Rem
    | Seven
    | Shiv
    | Silver
    | Sinclair
    | Venator
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
| "Apollo" -> Apollo
| "Bebop" -> Bebop
| "Billy" -> Billy
| "Calico" -> Calico
| "Celeste" -> Celeste
| "Doorman" -> Doorman
| "Drifter" -> Drifter
| "Dynamo" -> Dynamo
| "Graves" -> Graves
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
| "Rem" -> Rem
| "Seven" -> Seven
| "Shiv" -> Shiv
| "Silver" -> Silver
| "Sinclair" -> Sinclair
| "Venator" -> Venator
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
| Apollo -> "Apollo"
| Bebop -> "Bebop"
| Billy -> "Billy"
| Calico -> "Calico"
| Celeste -> "Celeste"
| Doorman -> "Doorman"
| Drifter -> "Drifter"
| Dynamo -> "Dynamo"
| Graves -> "Graves"
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
| Rem -> "Rem"
| Seven -> "Seven"
| Shiv -> "Shiv"
| Silver -> "Silver"
| Sinclair -> "Sinclair"
| Venator -> "Venator"
| Victor -> "Victor"
| Vindicta -> "Vindicta"
| Viscous -> "Viscous"
| Vyper -> "Vyper"
| Warden -> "Warden"
| Wraith -> "Wraith"
| Yamato -> "Yamato"

let is_frontliner = function
| Abrams -> true
| Apollo -> true
| Billy -> true
| Bebop -> true
| Calico -> true
| Celeste -> false
| Doorman -> false
| Drifter -> false
| Dynamo -> false
| Graves -> false
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
| Rem -> false
| Seven -> false
| Shiv -> true
| Silver -> true
| Sinclair -> false
| Venator -> true
| Victor -> true
| Vindicta -> false
| Viscous -> false
| Vyper -> false
| Warden -> true
| Wraith -> false
| Yamato -> true

let is_carry = function
| Abrams -> false
| Apollo -> false
| Billy -> false
| Bebop -> false
| Calico -> false
| Celeste -> true
| Doorman -> false
| Drifter -> false
| Dynamo -> false
| Graves -> true
| Grey_Talon -> false
| Haze -> false
| Holliday -> false
| Infernus -> true
| Ivy -> false
| Kelvin -> false
| Lady_Geist -> true
| Lash -> false
| McGinnis -> false
| Mina -> true
| Mirage -> true
| Mo_Krill -> false
| Paige -> false
| Paradox -> false
| Pocket -> false
| Seven -> true
| Rem -> false
| Shiv -> false
| Silver -> true
| Sinclair -> false
| Venator -> true
| Victor -> true
| Vindicta -> false
| Viscous -> false
| Vyper -> true
| Warden -> true
| Wraith -> true
| Yamato -> false

let is_pick = function
| Abrams -> false
| Apollo -> false
| Billy -> false
| Bebop -> true
| Calico -> false
| Celeste -> false
| Doorman -> true
| Drifter -> false
| Dynamo -> false
| Graves -> false
| Grey_Talon -> false
| Haze -> true
| Holliday -> true
| Infernus -> false
| Ivy -> false
| Kelvin -> false
| Lady_Geist -> false
| Lash -> true
| McGinnis -> false
| Mina -> false
| Mirage -> false
| Mo_Krill -> true
| Paige -> false
| Paradox -> true
| Pocket -> false
| Rem -> false
| Seven -> false
| Shiv -> false
| Silver -> false
| Sinclair -> false
| Venator -> false
| Victor -> false
| Vindicta -> false
| Viscous -> false
| Vyper -> false
| Warden -> false
| Wraith -> false
| Yamato -> false

let is_teamfighter = function
| Abrams -> false
| Apollo -> false
| Billy -> true
| Bebop -> false
| Calico -> false
| Celeste -> true
| Doorman -> false
| Drifter -> false
| Dynamo -> true
| Graves -> true
| Grey_Talon -> false
| Haze -> false
| Holliday -> false
| Infernus -> false
| Ivy -> false
| Kelvin -> true
| Lady_Geist -> false
| Lash -> true
| McGinnis -> false
| Mina -> false
| Mirage -> false
| Mo_Krill -> false
| Paige -> false
| Paradox -> false
| Pocket -> true
| Rem -> true
| Seven -> true
| Shiv -> false
| Silver -> false
| Sinclair -> true
| Venator -> false
| Victor -> true
| Vindicta -> false
| Viscous -> true
| Vyper -> false
| Warden -> true
| Wraith -> false
| Yamato -> false

let is_low_agency = function
| Abrams -> false
| Apollo -> false
| Billy -> false
| Bebop -> false
| Calico -> false
| Celeste -> false
| Doorman -> true
| Drifter -> false
| Dynamo -> true
| Graves -> true
| Grey_Talon -> true
| Haze -> false
| Holliday -> true
| Infernus -> false
| Ivy -> true
| Kelvin -> true
| Lady_Geist -> false
| Lash -> true
| McGinnis -> true
| Mina -> false
| Mirage -> false
| Mo_Krill -> true
| Paige -> true
| Paradox -> false
| Pocket -> false
| Rem -> true
| Seven -> true
| Shiv -> false
| Silver -> false
| Sinclair -> true
| Venator -> false
| Victor -> true
| Vindicta -> true
| Viscous -> true
| Vyper -> false
| Warden -> false
| Wraith -> false
| Yamato -> false

module Set = struct
  include Set.Make (T)
  include Provide_hash (T)
end

module Map = Map.Make (T)
module Table = Hashtbl.Make (T)

let all_set = Set.of_list all
