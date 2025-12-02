#!/usr/bin/env fennel

" Advent of Code 2025. Day 01. Solution by Darren Stone. "

(local util (include :util))

(fn relative-clicks [rotation]
	" Return positve :R or negative :L clicks, given rotation string. "
	(*	(tonumber (string.sub rotation 2))
		(. {:R 1 :L -1} (string.sub rotation 1 1))))

(fn rotate-part-one [starting-at relative-clicks]
	" Start at given position, rotate clicks (+/-), return final position. "
	(% (+ starting-at relative-clicks) 100))

(fn rotate-part-two [starting-at relative-clicks]
	" Start at given position, rotate clicks (+/-), count each zero hit
	  (in passing or final). Return values: zeros final-position. "
	(let [step (util.sign relative-clicks)]
		(var zeros 0)
		(var at starting-at)
		(for [_ step relative-clicks step]
			(set at (% (+ at step) 100))
			(if (= at 0) (set zeros (+ zeros 1))))
		(values zeros at)))

(var pointing-at 50) ; Part One start
(var zero-count 0) ; # zeros @ end of rotation

(each [_ rotation (ipairs (util.file-read-lines "input.txt"))]
	(let [relative-clicks (relative-clicks rotation)]
		(set pointing-at (rotate-part-one pointing-at relative-clicks))
		(if (= pointing-at 0)
			(set zero-count (+ zero-count 1)))))

(print (.. "Part One: " zero-count))

(set pointing-at 50) ; Part Two start
(set zero-count 0) ; # zeros (in passing or final)

(each [_ rotation (ipairs (util.file-read-lines "input.txt"))]
	(let [relative-clicks (relative-clicks rotation)
		  (total-zeros final-position) (rotate-part-two pointing-at relative-clicks)]
		(set pointing-at final-position)
		(set zero-count (+ zero-count total-zeros))))

(print (.. "Part Two: " zero-count))