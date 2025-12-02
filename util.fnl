; Advent of Code 2025. Utility functions by Darren Stone <dstone at bitmason dot com>.

(local M {})

(fn M.strf [...]
  " Lua (C-ish) format. "
  (string.format ...))

(fn M.writef [...] 
  " Lua (C-ish) format. "
  (io.write (M.strf ...))
  (io.flush))

(fn M.string-strip [str]
	" Strip whitespace. "
    (string.match str "^%s*(.-)%s*$"))

; NOTE: These two parsing routines (csv and other value) are probably overkill for
; Advent of Code but I had them for other projects and they are seemingly bug-free.

(fn M.csv-to-table [s conversion-function]
  " Return table of values from CSV string.
  	Quotes are optional around values but if supplied then all chars
  	withint quotes are preserved and those quotes are discarded.
  	Empty fields are preserved (e.g. 3,4,,6) UNLESS the conversion-function
  	returns nil (then the field is discarded).
   	Whitespace outside/between values is discarded.
   	If conversion-function supplied, it is applied to each value. "
  (let [  out {}
          convert (fn [v] (if conversion-function (conversion-function v) v))]
    (var i 1)
    (let [s-length (string.len s)]
      (var field "")
      (var in-quotes false)
      (var field-quoted false)

      (fn push-field [] ; insert helper
        (let [value (if field-quoted
                        field ; quoted so keep as-is
                        (M.string-strip field))] ; unquoted so strip whitespace
          (table.insert out (convert value))
          (set field "")
          (set field-quoted false)))

      (while (<= i s-length)
        (let [c (string.sub s i i)]
          (if in-quotes
              ; within quoted field
              (if (= c "\"") ; escaped quote or closing quote
                  (if (and (< i s-length)
                           (= (string.sub s (+ i 1) (+ i 1)) "\""))
                      (do (set field (.. field "\""))
                          (set i (+ i 1))) ; skip second quote
                      (do (set in-quotes false)
                          (set field-quoted true)))
                  ; regular character
                  (set field (.. field c)))
              ; not in quotes
              (if (= c ",") ; watch for end
                  (push-field) 
                  (if (= c "\"")
                      ; start of quoted field
                      (set in-quotes true)
                      (if (string.match c "%s") 
                          (if (and (= field "") (not field-quoted)) ; whitespace outside quotes
                              nil ; field empty; skip
                              (set field (.. field c))) ; keep internal whitespace
                          ; regular character
                          (set field (.. field c)))))))
        (set i (+ i 1)))
      ; preserve last field (could be empty)
      (push-field))
  out))

(fn M.string-to-table [s delimiter conversion-function]
  " Return table of values from delimited string.
  	Default delimiter is space.
   	For space: runs of spaces are considered a single delimiter.
    For other delimiters: split on delimiter, discard whitespace surrounding values, drop empty fields.    
    NOTE: If a field is in quotes, the quotes are not stripped in the returned value.
   	If conversion-function supplied, it is applied to each value. "
  (let [  delimiter (or delimiter " ")
          out {}
          convert (fn [v] (if conversion-function (conversion-function v) v))]
    (if (= delimiter " ")
        (each [field (string.gmatch s "%S+")] ; keep runs of non-space
          (table.insert out (convert field)))
        (do (var start 1) ; other delimiter
            (let [delimiter-length (string.len delimiter)]
              (var done false)
              (while (not done)
                (let [i (string.find s delimiter start true)]
                  (if i
                      (let [field (M.string-strip (string.sub s start (- i 1)))]
                        ; discard empty field
                        (if (not (= field ""))
                          (table.insert out (convert field)))
                        (set start (+ i delimiter-length)))
                      (do (let [field (M.string-strip (string.sub s start))]
                            (if (not (= field ""))
                              (table.insert out (convert field))))
                          (set done true))))))))
  out))

(fn M.file-read-lines [filename]
	" Return table of strings, one for each line of the given file. "
	(let [strings {}
          file (assert (io.open filename "r") (.. "Can't open file: " filename))]
    	(each [line (file:lines)] 
    		(table.insert strings line))
    	(file:close)
    	strings))

(fn M.file-read-lines-of-csv [filename conversion-function]
	" Return table of tables: one table of command separated values per line of the given file. 
	  See csv-to-table for more. "
	(let [tables {}
          file (assert (io.open filename "r") (.. "Can't open file: " filename))]
    	(each [line (file:lines)] 
    		(table.insert tables (M.csv-to-table line conversion-function)))
    	(file:close)
    	tables))

(fn M.file-read-lines-of-values [filename delimiter conversion-function]
	" Return table of tables: one table of values per line of the given file. 
	  See string-to-table for more. "
	(let [tables {}
          file (assert (io.open filename "r") (.. "Can't open file: " filename))]
    	(each [line (file:lines)] 
    		(table.insert tables (M.string-to-table line delimiter conversion-function)))
    	(file:close)
    	tables))

(fn M.divmod [a b]
	" Return (f m) for a/b,
	  where f is floor of the integer division and m is the remainder."
	(values (// a b) (% a b)))

(fn M.sign [n zero-value]
	" Return -1 or +1. By default, return +1 for n=0, otherwise use zero-value (default +1). "
	(if (= n 0) (or zero-value 1)
		(< n 0) -1
		1))

M