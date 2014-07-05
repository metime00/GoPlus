module GameOptions
/// What extra features are generated at board creation
type GenOption =
    | Regular
    | NeutralGen
    | Powerup
    | NeutralAndPowerup
/// Level of powerups randomly generated during play
type PowerOption =
    | Vanilla
    | Low
    | Medium
    | High