#!/usr/bin/env fennel

" Advent of Code 2025. Day 04. Solution by Darren Stone. "

(local util (include :util))

; read in the map
(local lines (util.file-read-lines "input.txt"))
(local map [])
(for [y 1 (length lines)]
    (tset map y [])
    (for [x 1 (length (. lines 1))]
        (tset (. map y) x (string.sub (. lines y) x x))))

(fn map-width [map] (length (. map 1)))

(fn map-height [map] (length map))

(fn at [map x y] (. (. map y) x))

(fn set-at [map x y v] (tset (. map y) x v))

(fn map-print [map]
    (for [y 1 (map-height map)]
        (for [x 1 (map-width map)]
            (util.writef "%s" (at map x y)))
        (util.writef "\n")))

(fn map-copy-deep [map]
    (local new-map [])
    (for [y 1 (map-height map)]
        (tset new-map y [])
        (for [x 1 (map-width map)]
            (tset (. new-map y) x (at map x y))))
    new-map)

(fn empty-at [map x y] 
    (if (or (< x 1) (> x (map-width map)) (< y 1) (> y (map-height map))) ; out of bounds = empty
        true
        (= "." (at map x y))))

(local adj-offsets [
    [-1 -1] [0 -1] [1 -1]
    [-1 0]         [1 0]
    [-1 1]  [0 1]  [1 1]])

(fn filled-adj-to [map x y]
    (var count 0)
    (each [_ [dx dy] (ipairs adj-offsets)]
        (if (not (empty-at map (+ x dx) (+ y dy)))
            (set count (+ count 1))))
    count)

(fn remove-accessible [map]
    " Remove accessible paper rolls. Return values: (removed new-map)  "
    (var removed 0)
    (local new-map (map-copy-deep map))
    (for [y 1 (map-height map)]
        (for [x 1 (map-width map)]
            (when (and (not (empty-at map x y))
                    (< (filled-adj-to map x y) 4))
                (set-at new-map x y ".")
                (set removed (+ removed 1)))))
    (values removed new-map))

(local map-part-one (map-copy-deep map))
(util.writef "Part One: %d\n" (remove-accessible map-part-one))

(var map-cur (map-copy-deep map))
(var removed 0)
(var continue true)
(while continue
    (let [(last-removed map-new) (remove-accessible map-cur)]
        (set removed (+ removed last-removed))
        (set continue (~= last-removed 0))
        (set map-cur (map-copy-deep map-new))))
(util.writef "Part Two: %d\n" removed)