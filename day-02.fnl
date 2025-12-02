#!/usr/bin/env fennel

" Advent of Code 2025. Day 02. Solution by Darren Stone. "

(local util (include :util))

(local ranges (. (util.file-read-lines-of-csv "input.txt") 1))

(fn invalid-one? [id]
	" First half matches second half? "
	(let [	id-string (tostring id)
			id-length (length id-string)]
		(=	(string.sub id-string 1 (// id-length 2))
			(string.sub id-string (+ (// id-length 2) 1)))))

(var part-one 0)
(each [_ range (ipairs ranges)]
	(let [[first last] (util.string-to-table range "-" tonumber)]
		(for [id first last]
			(if (invalid-one? id)
				(set part-one (+ part-one id))))))

(util.writef "Part One: %d\n" part-one)

(fn invalid-two? [id]
	" For each possible pattern length (i.e. up to half id-length),
	  repeat that prefix pattern and compare to the original id. "
	(var invalid false)
	(let [	id-string (tostring id)
			id-length (length id-string)]
		(for [pattern-length 1 (// id-length 2) &until invalid]
			(let [pattern (string.sub id-string 1 pattern-length)]
				(if (= id-string (string.rep pattern (// id-length pattern-length)))
					(set invalid true)))))
	invalid)

(var part-two 0)
(each [_ range (ipairs ranges)]
	(let [[first last] (util.string-to-table range "-" tonumber)]
		(for [id first last]
			(if (invalid-two? id)
				(set part-two (+ part-two id))))))

(util.writef "Part Two: %d\n" part-two)
