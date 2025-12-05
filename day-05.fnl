#!/usr/bin/env fennel

" Advent of Code 2025. Day 05. Solution by Darren Stone. "

(local util (include :util))

(local lines (util.file-read-lines "input.txt"))

(var done-fresh false)
(var fresh-ranges []) ; read fresh ranges
(for [i 1 (length lines) &until done-fresh]
    (let [line (. lines i)]
        (if (= line "")
            (set done-fresh true)
            (let [[from to] (util.string-to-table line "-")]
                (tset fresh-ranges (+ (length fresh-ranges) 1) [(tonumber from) (tonumber to)])))))

(var available []) ; read available IDs
(for [i (+ (length fresh-ranges) 2) (length lines)]
    (tset available (+ (length available) 1) (tonumber (. lines i))))

(fn fresh? [id]
    (var found false)
    (each [_ [from to] (ipairs fresh-ranges)]
        (when (and (not found) (>= id from) (<= id to))
            ;(print id from to)
            (set found true)))
    found)

(fn range-length [from to] (+ (- to from) 1))

(fn overlap-length [from0 to0 from1 to1]
    (if (and (<= from0 from1) (>= to0 from1)) (range-length from1 to0) ; from0 from1 to0 to1
        (and (<= from1 from0) (>= to1 from0)) (range-length from0 to1) ; from1 from0 to1 to0
        (and (>= from0 from1) (<= to0 to1)) (range-length from0 to0) ; from1 from0 to0 to1
        (and (>= from1 from0) (<= to1 to0)) (range-length from1 to1) ; from0 from1 to1 to0
        0))

(var part-one 0)
(each [_ id (ipairs available)]
    (if (fresh? id) (set part-one (+ part-one 1))))
(util.writef "Part One: %d\n" part-one)

(var part-two 0)
(for [i 1 (length fresh-ranges)]
    (let [[i0 i1] (. fresh-ranges i)
          id-count (range-length i0 i1)] ; potential, subject to removing overlaps below
        (set part-two (+ part-two id-count))
        (for [j (+ i 1) (length fresh-ranges)]
            (let [[j0 j1] (. fresh-ranges j)
                  dupe-count (overlap-length i0 i1 j0 j1)]
                (set part-two (- part-two dupe-count))))))
(util.writef "Part Two: %d\n" part-two)
