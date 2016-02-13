module GameOptions
/// What extra features are generated at board creation
type GenOption =
    {
        NeutralGen: bool
    }
/// Level of powerups randomly generated during play, corresponding to a float that if a random float is less than, a powerup is placed
type PowerOption =
    | Vanilla
    | Low //95% chance of 1 powerup per 100 turns, success rate per turn is 0.029513
    | Medium //95% chance of 1 powerup per 50 turns, success rate per turn is 0.058155
    | High //95% chance of 1 powerup per 25 turns, success rate per turn is 0.112928