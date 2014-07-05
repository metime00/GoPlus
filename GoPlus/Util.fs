module Util

let inline boundCheck (x, y) width height = x >= 0 && x < width && y >= 0 && y < height