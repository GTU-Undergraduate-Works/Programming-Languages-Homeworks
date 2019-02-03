(load "lexer.lisp")

(setq index 0)


(defun add-child (tree child)
  (setf (car tree) (append (car tree) child))
  tree)

(defun make-tree (data)
  "Creates a new node that contains 'data' as its data."
  (cons (cons data nil) nil))



(defun INPUT(lst)
	(cond
		((is_input_exist lst) (add-child (add-child (make-tree "INPUT") (EXPI lst)) (INPUT_helper lst)))
		(t nil)))


(defun INPUT_helper(lst)
	(cond
		((is_input_exist lst) (EXPLISTI lst))
		(t nil)))


(defun EXPLISTI(lst)
	(cond
		((string= "null" (get_token lst index)) (setq index (+ 1 index)) (add-child (make-tree "EXPLISTI") "null"))
		(t (add-child (make-tree "EXPLISTI") (car (EXPLISTI_helper lst))))))


(defun isEXPLIST(lst)
	(cond
		((string= "(" (get_token lst index))
			(cond
				((or (string= "concat" (get_token lst (+ index 1))) (string= "append" (get_token lst (+ index 1)))) t))
			(t nil))
		((string= "'(" (get_token lst index)) t)
		(t nil)))


 
(defun EXPLISTI_helper(lst)

	(cond
    	((and (is_input_exist lst) (string/= ")" (get_token lst index)) (not (null (setq tree (add-child (car (make-tree (EXPI lst))) (car (EXPLISTI_helper lst))))))) tree)
    	(t nil)))



(defun EXPI(lst)
	(cond
		((string/= "(" (get_token lst index)) 
			(cond
				((string= "identifier" (get_token_class lst index)) (setq exp_tree (add-child (make-tree "EXPI") (add-child (make-tree "ID") (list (list (get_token lst index)))))) (setq index (+ 1 index)) exp_tree)

				((string= "integer" (get_token_class lst index))  (setq exp_tree (add-child (make-tree "EXPI") (add-child (make-tree "INTEGER") (list (list (get_token lst index)))))) (setq index (+ 1 index)) exp_tree)
				(t nil)))
		(t  (setq index (+ 1 index)) (setq tree (add-child (make-tree "(") (car (EXPI_helper lst))))
			(cond
				((and (not (null tree)) (string= ")" (get_token lst index)))  (setq index (+ 1 index)) (add-child (make-tree "EXPI") (add-child tree (car (make-tree ")"))))) ; buraya dikkat et sıkıntı çıkabilir.
				(t nil)))))






(defun EXPI_helper(lst)
	(cond
		((string= "operator" (get_token_class lst index)) (setq tree (make-tree (get_token lst index))) (setq index (+ 1 index)) 
			 (add-child (add-child tree (EXPI lst)) (EXPI lst)))
		((string= "identifier" (get_token_class lst index)) (setq tree (make-tree (get_token lst index))) (setq index (+ 1 index)) (add-child tree (EXPLISTI lst)))
		((string= "deffun" (get_token lst index)) (setq tree (make-tree (get_token lst index))) (setq index (+ 1 index)) 
				(cond
					((string= "identifier" (get_token_class lst index)) (setq tree (add-child tree (car (ID lst)))) 
						(setq temp_tree (car (IDLIST lst)))
						(cond
							((not (null temp_tree)) (add-child (add-child tree temp_tree) (car (EXPLISTI lst)))))))



			)
		((or (string= "set" (get_token lst index)) (string= "defvar" (get_token lst index))) (setq index (+ 1 index))


			(cond
				((string= "identifier" (get_token_class lst index)) (setq index (+ 1 index))
					(add-child (add-child (make-tree (get_token lst (- index 2))) (list (get_token lst (- index 1)))) (EXPI lst)))
				(t nil)))
		((string= "if" (get_token lst index)) (setq index (+ 1 index))


			(add-child (add-child (make-tree"if") (EXPB lst)) (EXPLISTI lst)))
		((string= "while" (get_token lst index))   (setq index (+ 2 index))  (setq e_tree (add-child (add-child (add-child (make-tree "while") (car (make-tree "("))) (EXPB lst)) (car (make-tree ")")))) (setq index (+ 1 index)) (add-child e_tree (EXPLISTI lst)))
		((string= "for" (get_token lst index)) (setq index (+ 2 index)) (setq ef_tree (add-child (add-child (add-child (add-child (make-tree "for") (car (make-tree "("))) (ID lst)) (EXPI lst)) (make-tree ")"))) )
		(t (setq index (+ 1 index)) nil)))



(defun EXPB(lst)
	(cond 
		((string/= "(" (get_token lst index)) 
			(cond
				((string= "binary value" (get_token_class lst index)) (setq index (+ 1 index)) (add-child (make-tree "EXPB") (add-child (make-tree "BINARY VALUE") (list (list (get_token lst (- index 1)))))))))
		(t (setq index (+ 1 index))  (setq temp_tree (add-child (make-tree "(") (car (EXPB_helper lst)))) 
			(cond
				((and (not (null temp_tree)) (string= ")" (get_token lst index))) (setq index (+ 1 index)) (add-child (make-tree "EXPB") (add-child temp_tree (car (make-tree ")")))))
				(t nil)))
	))



(defun EXPB_helper(lst)
	(cond
		((or (string= "and" (get_token lst index)) (string= "or" (get_token lst index))) (setq tree (make-tree (get_token lst index))) (setq index (+ 1 index))
			(setq temp_tree (EXPB lst))
			(cond
				((not (null temp_tree)) (setq tree (add-child tree temp_tree)) (setq temp_tree (EXPB lst))
					(cond
						((not (null temp_tree)) (add-child tree temp_tree))
						(t nil)))
				(t nil)))
		((string= "not" (get_token lst index)) (setq index (+ 1 index)) (add-child (make-tree "not") (EXPB lst)))

		((string= "equal" (get_token lst index)) (setq index (+ 1 index))
			(cond 
				((isEXPB lst) (add-child (add-child (make-tree "equal") (EXPB lst)) (EXPB lst)))
				(t (add-child (add-child (make-tree "equal") (EXPI lst)) (EXPI lst)))))
		(t nil)
		))





(defun isEXPB(lst)
	(cond
		((string= "binary value" (get_token_class lst index)) t)
		((string= "(" (get_token lst index))
			(cond
				((or (string= "and" (get_token lst (+ 1 index))) (string= "or" (get_token lst (+ 1 index)))
					(string= "not" (get_token lst (+ 1 index))) (string= "equal" (get_token lst (+ 1 index)))) t)
				(t nil)))
		(t nil)))


(defun is_input_exist(lst)
	(cond
		((or (< index 0) (>= index (length lst))) nil)
	(t t)))
	

(defun get_token_class(lst index)
	(cond
		((and (< index 0) (>= index (length lst))))
		(t (car (nth index lst)))))


(defun get_token(lst index)
	(cond
		((and (< index 0) (>= index (length lst))))
		(t (car (cdr (nth index lst))))))


(defun IDLIST(lst)
	(setq exp_tree (make-tree "IDLIST"))
	(cond
		((not (is_input_exist lst)) nil)
		((string= "(" (get_token lst index))
			(setq index (+ 1 index))
			(setq exp_tree (add-child exp_tree (add-child (make-tree "(") (car (IDLIST lst))))))

		((string= "identifier" (get_token_class lst index)) (add-child (add-child exp_tree (ID lst)) (IDLIST lst)))
		((string= ")" (get_token lst index)) (setq index (+ 1 index)) (car (make-tree ")")))))



(defun ID(lst)
	(cond
		((string= "identifier" (get_token_class lst index)) (setq index (+ 1 index)) (add-child (make-tree "ID") (list (list (get_token lst (- index 1))))))
		(t nil)))



(defun create_string(number ch)
	(cond
		((equal 0 number) "")
		(t (concatenate 'string (string ch) (create_string (- number 1) ch)))))


(defun write_tree_to_file(parse_tree number stream)
	(cond
		((null parse_tree) nil)
		((atom (car parse_tree)) (format stream (create_string number #\Tab)) (format stream (car parse_tree)) (terpri stream) (write_tree_to_file (cdr parse_tree) number stream))
		(t (write_tree_to_file (car parse_tree) (+ 1 number) stream) (write_tree_to_file (cdr parse_tree) number stream))))



(defun parser(lst)
 (car (INPUT lst)))







; token listesi buraya parametre olarak verilmeli
(setq parse_tree (parser (reverse token_list)))



(with-open-file (stream "161044086.tree" :direction :output  :if-does-not-exist :create)

		(format stream "DIRECTIVE: parse tree") (terpri stream) (terpri stream)
		(write_tree_to_file parse_tree 0 stream)
		(close stream)

	)
