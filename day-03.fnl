#!/usr/bin/env fennel

" Advent of Code 2025. Day 03. Solution by Darren Stone. "

(local util (include :util))

(local banks (util.file-read-lines "input.txt"))

(fn pos-start-best [bank n]
    " Index into bank string where largest number of n digits must start. "
    (var pos-best 1)
    (var d-max (string.sub bank pos-best pos-best))
    (for [pos 1 (+ (- (length bank) n) 1)]
        (let [d (string.sub bank pos pos)]
            (when (> d d-max)
                (set d-max d)
                (set pos-best pos))))
    pos-best)

(fn joltage-max [bank n]
    " Largest numeric value of n digits (in string form) from given string bank of digits. "
    (if (= n 1) (util.string-char-max bank)
        (= (length bank) n) bank    
        (let [pos-start (pos-start-best bank n)]
            (.. (string.sub bank pos-start pos-start) 
                (joltage-max (string.sub bank (+ pos-start 1)) (- n 1))))))

(var part-one 0)
(each [_ bank (ipairs banks)]
    (set part-one (+ part-one (joltage-max bank 2)))) 
(util.writef "Part One: %d\n" part-one)

(var part-two 0)
(each [_ bank (ipairs banks)]
    (set part-two (+ part-two (joltage-max bank 12))))
(util.writef "Part Two: %d\n" part-two)