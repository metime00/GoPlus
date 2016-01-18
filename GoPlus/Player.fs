module Player
open Util
open Pieces

type Player = {
    color : Color;
    score : int;
    powerup : Option<Powerup> ;
    }