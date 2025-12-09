#!/usr/bin/env fennel

; Advent of Code 2025. Day 07. Solution by Darren Stone.

(local u (include :util))
(local filename "input.txt")

; Part One
(var map (u.file-read-array2d-chars filename))
(var splits 0)
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
                (set splits (+ splits 1))
                (if (= (u.array2d-get map (- x 1) y) ".")
                        (u.array2d-set map (- x 1) y "|"))
                (if (= (u.array2d-get map (+ x 1) y) ".")
                        (u.array2d-set map (+ x 1) y "|"))))))
(u.writef "Part One: %d\n" splits)

; Part Two
(var ymax (- (u.array2d-height map) 1)) ; last row of splitters
(var timelines-memo {}) ; "[start-x],[start-y]" -> previously computed value of (timelines start-x start-y)
(fn xy-label [x y] (u.strf "%d,%d" x y))

(fn timelines [start-x start-y] ; how many timelines starting at x y (may be beam or splitter)
    (let [total-recalled (. timelines-memo (xy-label start-x start-y))]
        (or total-recalled
            (do ; compute from scratch
                (var [x y] [start-x start-y])
                (while (and (< y ymax) (~= (u.array2d-get map x y) "^")) ; follow beam until splitter
                     (set y (+ y 1)))
                (local total (if (>= y ymax)
                                (if (= (u.array2d-get map x y) "^")
                                    2
                                    1)
                                (+ (timelines (- x 1) y) (timelines (+ x 1) y)))) ; expensive recursion (thus, the memoize)
                (tset timelines-memo (xy-label start-x start-y) total) ; memoize
                total))))
(u.writef "Part Two: %d\n" (timelines x1 y1))