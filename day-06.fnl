#!/usr/bin/env fennel

" Advent of Code 2025. Day 06. Solution by Darren Stone. "

(local util (include :util))

(local filename "input.txt")
(local numbers (let [raw (util.file-read-lines-of-values filename " " tonumber)] (table.remove raw (length raw)) raw))
(local operators (let [raw (util.file-read-lines-of-values filename)] (. raw (length raw))))
(local num-cols (length (. numbers 1)))

(var answers [])
(var col-op nil)
(for [col 1 num-cols]
    (tset answers col 
        (if (= (. operators col) "+")
                (do (set col-op (fn [a b] (+ a b)))
                    0)
            (= (. operators col) "*")
                (do (set col-op (fn [a b] (* a b)))
                    1)))
    (for [row 1 (length numbers)]
        (tset answers col (col-op (. answers col) (. (. numbers row) col)))))
(var part-one 0)
(for [col 1 num-cols] (set part-one (+ part-one (. answers col))))
(util.writef "Part One: %d\n" part-one)

(local lines (util.file-read-lines filename))
(local number-lines (let [raw (util.file-read-lines filename)] (table.remove raw (length raw)) raw))
(var col-starts [])
(var col-ends [])
(local operators-raw (. lines (length lines)))
(for [c 1 (length operators-raw)]
    (when (~= (string.sub operators-raw c c) " ")
        (table.insert col-starts c)
        (if (~= c 1)
            (table.insert col-ends (- c 2)))))
(table.insert col-ends (length operators-raw))

(set answers [])
(for [col 1 num-cols]
    (tset answers col 
        (if (= (. operators col) "+")
                (do (set col-op (fn [a b] (+ a b)))
                    0)
            (= (. operators col) "*")
                (do (set col-op (fn [a b] (* a b)))
                    1)))
    (for [c (. col-starts col) (. col-ends col)]
        (var c-number "")
        (for [r 1 (length number-lines)]
            (set c-number (.. c-number (string.sub (. number-lines r) c c))))
        (tset answers col (col-op (. answers col) (tonumber c-number)))))
(var part-two 0)
(for [col 1 num-cols] (set part-two (+ part-two (. answers col))))
(util.writef "Part Two: %d\n" part-two)
