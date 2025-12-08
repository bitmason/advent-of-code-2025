#!/usr/bin/env fennel

; Advent of Code 2025. Day 07. Solution by Darren Stone.

(local u (include :util))
(local filename "input.txt")

; Part One
(var map (u.file-read-array2d-chars filename))
(var splits [])
(var [x1 y1] [nil nil]) ; first splitter
(for [y 1 (u.array2d-height map)]
    (for [x 1 (u.array2d-width map)]
        (let [c (u.array2d-get map x y)]
            (when (= c "S")
                (u.array2d-set map x (+ y 1) "|" )
                (set [x1 y1] [x (+ y 2)]))
            (when (and (> y 1) (= c ".") (= (u.array2d-get map x (- y 1)) "|"))
                (u.array2d-set map x y "|" ))
            (when (and (= c "^") (= (u.array2d-get map x (- y 1)) "|"))
                (table.insert splits {(xy-label x y) :U})
                (if (= (u.array2d-get map (- x 1) y) ".")
                        (u.array2d-set map (- x 1) y "|"))
                (if (= (u.array2d-get map (+ x 1) y) ".")
                        (u.array2d-set map (+ x 1) y "|"))))))
(u.writef "Part One: %s\n" (length splits))
