#!/usr/bin/env fennel

; Advent of Code 2025. Day 08. Solution by Darren Stone.

(local u (include :util))
(local filename "input.txt")

(local box-locns (u.file-read-lines-of-csv filename (fn [n] (assert (tonumber n)))))
(local box-count (length box-locns))

(fn dist-xyz [xyz1 xyz2] ; straight line distance (each xyz=[x y z])
    (let [[x1 y1 z1] xyz1
          [x2 y2 z2] xyz2]
        (math.sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)) (* (- z2 z1) (- z2 z1))))))

(var dist-ij-sparse []) ; sparse flattened upper triangular matrix of distance from box i to j (look up with dist-ij function)
(var dist-shortest []) ; each dist from dist-ij-sparse with i j tag: [i j dist]
(for [i 1 box-count]
    (for [j (+ i 1) box-count]
        (let [dist (dist-xyz (. box-locns i) (. box-locns j))]
            (table.insert dist-ij-sparse dist)
            (table.insert dist-shortest [i j dist]))))
(fn sort-ijd [a b]
    (let [[_ _ dist1] a
          [_ _ dist2] b]
    (< dist1 dist2)))
(table.sort dist-shortest sort-ijd)

(fn dist-ij [i j] ; straight line distance from box i to box j (already in dist-ij-sparse matrix)
    (let [[a b] (if (< i j) [i j] [j i])]
        (. dist-ij-sparse (+ ; upper triangular sparse matrix access
            (* (- a 1) box-count)
            (- b a)
            (- (/ (* (- a 1) a) 2))))))

(var circuits []) ; list of list of boxes on the same circuit e.g. [ [1 20 8] [2 3] ... ]
(for [i 1 box-count]
    (table.insert circuits [i])) ; every box in its own circuit initially

(fn circuit-with-box [b] ; index of circuit in circuits containing box b (or nil if none)
    (var i-found nil)
    (each [i c (ipairs circuits) &until i-found]
        (if (u.table-contains? c b)
            (set i-found i)))
    i-found)

(fn connect-boxes [i j] ; connect box i to box j
    (let [i-circuit (circuit-with-box i)
          j-circuit (circuit-with-box j)]
        (if (= i-circuit j-circuit) 
            nil ; do nothing
            (do (each [_ box (pairs (. circuits j-circuit))] ;combine
                    (table.insert (. circuits i-circuit) box))
                (table.remove circuits j-circuit)))))

; Part One
(local part-one-pairs 1000)
(for [n 1 part-one-pairs]
    (let [[i j dist] (. dist-shortest n)]
        (connect-boxes i j)))
(table.sort circuits (fn [c1 c2] (> (length c1) (length c2))))
(u.writef "Part One: %d\n" (* (length (. circuits 1)) 
                              (length (. circuits 2)) 
                              (length (. circuits 3))))

; Part Two
(var pair (+ part-one-pairs 1))
(while (> (length circuits) 1)
    (let [[i j dist] (. dist-shortest pair)]
        (connect-boxes i j)
        (set pair (+ pair 1))))
(let [[i j _] (. dist-shortest (- pair 1))
      ix (. (. box-locns i) 1)
      jx (. (. box-locns j) 1)]
    (u.writef "Part Two: %d\n" (* ix jx)))