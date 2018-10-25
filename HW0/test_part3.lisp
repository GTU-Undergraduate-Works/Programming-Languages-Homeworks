(load "csv-parser.lisp")
(in-package :csv-parser)

;; (read-from-string STRING)
;; This function converts the input STRING to a lisp object.
;; In this code, I use this function to convert lists (in string format) from csv file to real lists.

;; (nth INDEX LIST)
;; This function allows us to access value at INDEX of LIST.
;; Example: (nth 0 '(a b c)) => a

;; !!! VERY VERY VERY IMPORTANT NOTE !!!
;; FOR EACH ARGUMENT IN CSV FILE
;; USE THE CODE (read-from-string (nth ARGUMENT-INDEX line))
;; Example: (mypart1-funct (read-from-string (nth 0 line)) (read-from-string (nth 1 line)))

(defun insert-n(lst number index)
	"This function takes a list, a number and an index
	as inputs, the inserts the number to the specified 
	index in the list"
	(cond ((or (< index 0) (> index (length lst))) lst)
		((= 0 index) (append (list number) lst))
		((= index (length lst)) (append lst (list number)))
		(t (append (list (car lst)) (insert-n (cdr lst) number (- index 1)))))
	)



;; MAIN FUNCTION
(defun main ()
  (with-open-file (stream #p"input_part3.csv")
    (loop :for line := (read-csv-line stream) :while line :collect
      (format t "~a~%"
      
      	(insert-n (read-from-string (nth 0 line)) (read-from-string (nth 1 line)) (read-from-string (nth 2 line)))



      )
    )
  )
)

;; CALL MAIN FUNCTION
(main)
