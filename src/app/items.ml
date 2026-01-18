open! Core

module T4 = struct
  type t =
    | Armor_piercing_rounds
    | Capacitor
    | Crippling_Headshot
    | Crushing_Fists
    | Frenzy
    | Glass_Cannon
    | Lucky_Shot
    | Ricochet
    | Shadow_Weave
    | Silencer
    | Spellslinger
    | Spiritual_Overflow
    | Cheat_Death
    | Colossus
    | Divine_Barrier
    | Diviners_Kevlar
    | Healing_Tempo
    | Infuser
    | Inhibitor
    | Juggernaut
    | Leech
    | Phantom_Strike
    | Plated_Armor
    | Siphon_Bullets
    | Spellbreaker
    | Unstoppable
    | Vampiric_Burst
    | Witchmail
    | Arctic_Blast
    | Boundless_Spirit
    | Curse
    | Echo_Shard
    | Escalating_Exposure
    | Ethereal_Shift
    | Focus_Lens
    | Lightning_Scroll
    | Magic_Carpet
    | Mercurial_Magnum
    | Mystic_Reverb
    | Refresher
    | Scourge
    | Spirit_Burn
    | Transcendant_Cooldown
    | Vortex_Web
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
  | Armor_piercing_rounds -> "Armor piercing rounds"
  | Capacitor -> "Capacitor"
  | Crippling_Headshot -> "Crippling Headshot"
  | Crushing_Fists -> "Crushing Fists"
  | Frenzy -> "Frenzy"
  | Glass_Cannon -> "Glass Cannon"
  | Lucky_Shot -> "Lucky Shot"
  | Ricochet -> "Ricochet"
  | Shadow_Weave -> "Shadow Weave"
  | Silencer -> "Silencer"
  | Spellslinger -> "Spellslinger"
  | Spiritual_Overflow -> "Spiritual Overflow"
  | Cheat_Death -> "Cheat Death"
  | Colossus -> "Colossus"
  | Divine_Barrier -> "Divine Barrier"
  | Diviners_Kevlar -> "Diviner's Kevlar"
  | Healing_Tempo -> "Healing Tempo"
  | Infuser -> "Infuser"
  | Inhibitor -> "Inhibitor"
  | Juggernaut -> "Juggernaut"
  | Leech -> "Leech"
  | Phantom_Strike -> "Phantom Strike"
  | Plated_Armor -> "Plated Armor"
  | Siphon_Bullets -> "Siphon Bullets"
  | Spellbreaker -> "Spellbreaker"
  | Unstoppable -> "Unstoppable"
  | Vampiric_Burst -> "Vampiric Burst"
  | Witchmail -> "Witchmail"
  | Arctic_Blast -> "Arctic Blast"
  | Boundless_Spirit -> "Boundless Spirit"
  | Curse -> "Curse"
  | Echo_Shard -> "Echo Shard"
  | Escalating_Exposure -> "Escalating Exposure"
  | Ethereal_Shift -> "Ethereal Shift"
  | Focus_Lens -> "Focus Lens"
  | Lightning_Scroll -> "Lightning Scroll"
  | Magic_Carpet -> "Magic Carpet"
  | Mercurial_Magnum -> "Mercurial Magnum"
  | Mystic_Reverb -> "Mystic Reverb"
  | Refresher -> "Refresher"
  | Scourge -> "Scourge"
  | Spirit_Burn -> "Spirit Burn"
  | Transcendant_Cooldown -> "Transcendant Cooldown"
  | Vortex_Web -> "Vortex Web"
end
