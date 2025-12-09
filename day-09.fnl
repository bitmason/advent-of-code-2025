#!/usr/bin/env fennel

; Advent of Code 2025. Day 09. Solution by Darren Stone.

(local u (include :util))
(local filename "input.txt")

(local red-tiles (u.file-read-lines-of-csv filename (fn [n] (assert (tonumber n)))))
(local red-tile-count (length red-tiles))

(fn area-rect [x1 y1 x2 y2]
    (* (+ (math.abs (- x1 x2)) 1) (+ (math.abs (- y1 y2)) 1)))

(var area-max-1 0) ; Part One
(for [i 1 red-tile-count]
    (let [[xi yi] (. red-tiles i)]
        (for [j (+ i 1) red-tile-count]
            (let [[xj yj] (. red-tiles j)
                  area (area-rect xi yi xj yj)]
                (if (> area area-max-1)
                    (set area-max-1 area))))))
(u.writef "Part One: %d\n" area-max-1)