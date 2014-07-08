module Util

/// Returns true when the given coordinates are inside the bounds given
let boundCheck (x, y) width height = x >= 0 && x < width && y >= 0 && y < height
/// A board that is initially marked all not visited
let noVisits size = Array2D.create (size) (size) false