#!/usr/bin/env fennel

" Advent of Code 2025. Day 04. Solution by Darren Stone. "

(local util (include :util))

(local map (util.file-read-array2d-chars "input.txt"))

(fn empty-at [map x y] 
    (if (or (< x 1) (> x (util.array2d-width map)) (< y 1) (> y (util.array2d-height map))) ; out of bounds = empty
        true
        (= "." (util.array2d-get map x y))))

(fn filled-adj-to [map x y]
    " Number of non-empty positions adjacent to x,y. "
    (var count 0)
    (each [_ [dx dy] (ipairs util.array2d-adj-offsets)]
        (if (not (empty-at map (+ x dx) (+ y dy)))
            (set count (+ count 1))))
    count)

(fn remove-accessible [map]
    " Remove accessible paper rolls. Return values: (removed new-map)  "
    (var removed 0)
    (local new-map (util.array2d-copy-deep map))
    (for [y 1 (util.array2d-height map)]
        (for [x 1 (util.array2d-width map)]
            (when (and (not (empty-at map x y))
                    (< (filled-adj-to map x y) 4))
                (util.array2d-set new-map x y ".")
                (set removed (+ removed 1)))))
    (values removed new-map))

(local map-part-one (util.array2d-copy-deep map))
(util.writef "Part One: %d\n" (remove-accessible map-part-one))

(var map-cur (util.array2d-copy-deep map))
(var removed 0)
(var continue true)
(var generations 0)
(while continue
    (set generations (+ generations 1))
    (let [(last-removed map-new) (remove-accessible map-cur)]
        (set removed (+ removed last-removed))
        (set continue (~= last-removed 0))
        (set map-cur (util.array2d-copy-deep map-new))))
(util.writef "Part Two: %d\n" removed)