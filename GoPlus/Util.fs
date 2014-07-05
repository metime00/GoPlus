module Util

/// Returns true when the given coordinates are inside the bounds given
let boundCheck (x, y) width height = x >= 0 && x < width && y >= 0 && y < height