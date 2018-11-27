;; Lexer implementation of G++ language
;; G++ language syntax is like lips language syntax
;; 
;; Tokenize the given g++ language source code
;; and prints the tokens the on screen with token class
;; If there is an error in tokenization process, prints
;; the error message on screen.
;;
;; Author : Efkan Durakli
;;


(setf LETTER 0)
(setf DIGIT 1)
(setf UNKNOWN 99)


(setf ADD_OP 15)
(setf SUB_OP 16)
(setf MULT_OP 17)
(setf DIV_OP 18)
(setf LEFT_PAREN 19)
(setf RIGHT_PAREN 20)


(setq KEYWORDS '("and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit"))
(setq OPERATORS '("+" "-" "/" "*" "(" ")" "**"))
(setq BINARY_VALUES '("true" "false"))


(setq token_list '())
(setq lexeme "")


(defun lexer (filename)
	"Gets the filename as parameter and tokenize the file. 
	If there is an error in tokenization, prints Error message
	and returns."
	(setq word_list (remove_whitespaces_from_lst (get_word_list (get_line_list filename))))
	(cond
		((null (lexer_help word_list)) (print "LEXICAL ERROR !!!") (terpri)))
	(print (reverse token_list) (terpri))
	)


(defun lexer_help (lst)
	"Gets word list as parameter and recursively tokenize the words."
	(cond
		((null lst) t)
		((string= (car lst) "") (lexer_help (cdr lst)))
		((null (lex_str (car lst))) nil)
		(t (lexer_help (cdr lst))))
	)


(defun lex_str (str)
	"Tokenize the given string and adds the token to token list.
	If there is an error in tokenizationreturn nil,  if not return t."
	(cond
		((and (= 0 (length str)) (string/= "" lexeme)) (look_str lexeme))
		((and (= 0 (length str)) (string= "" lexeme)) t)
		(t 
			(cond
				((or (= LEFT_PAREN (look_up (char str 0))) (= RIGHT_PAREN (look_up (char str 0)))) 
					
					(cond
						((= 0 (length lexeme)) (setq token_list (push (append '("operator") (list (string (char str 0)))) token_list)) (lex_str (subseq str 1)))
						(t (and (look_str lexeme) (setq token_list (push (append '("operator") (list (string (char str 0)))) token_list)) (lex_str (subseq str 1))))))
				((/= UNKNOWN (look_up (char str 0))) (setq lexeme (concatenate 'string lexeme (string (char str 0)))) (lex_str (subseq str 1)))
				(t nil)))))


(defun look_str(str)
	"Checks given string is operator, keyword, binary value, identifier or integer
	and pushes the string token_list list. If string is not legal lexeme, return nil"
	(cond
		((= 0 (length str)) nil)
		((search_in_list str OPERATORS) (setq token_list (push (append '("operator") (list lexeme)) token_list)) (setq lexeme "") t)
		((search_in_list str KEYWORDS)  (setq token_list (push (append '("keyword") (list lexeme)) token_list)) (setq lexeme "") t)
		((search_in_list str BINARY_VALUES)  (setq token_list (push (append '("binary value") (list lexeme)) token_list))(setq lexeme "") t)
		((is_identifier str)  (setq token_list (push (append '("identifier") (list lexeme)) token_list)) (setq lexeme "") t)
		((is_integer str) (setq token_list (push (append '("integer") (list lexeme)) token_list))(setq lexeme "") t)
		(t (setq lexeme "") nil)))


(defun search_in_list (element lst)
	"Checks the element is in list or not."
	(cond
		((null lst) nil)
		((equal element (car lst)) t)
		(t (search_in_list element (cdr lst)))))


(defun is_digit(ch)
	"Checks the given character is digit or not."
	(cond
		((and (char>= ch #\0) (char<= ch #\9)) t)))


(defun is_letter(ch)
	"Checks the given character is letter or not."
	(alpha-char-p ch))


(defun look_up (ch)
	"Looks given character and return character class of given character."
	(cond
		((char= ch #\() LEFT_PAREN)
		((char= ch #\)) RIGHT_PAREN)
		((char= ch #\+) ADD_OP)
		((char= ch #\-) SUB_OP)
		((char= ch #\*) MULT_OP)
		((char= ch #\/) DIV_OP)
		((is_letter ch) LETTER)
		((is_digit ch) DIGIT)
		(t UNKNOWN)))


(defun is_identifier(str)
	"Checks the given string is legal identifier or not."
	(cond
		((= 0 (length str)) nil)
		((and (= 1 (length str)) (is_letter (char str 0))) t)
		((is_letter (char str 0)) (is_identifier (subseq str 1)))
		(t nil)))


(defun is_integer(str)
	"Checks the given string is integer or not."
	(cond
		((char= (char str 0)) (is_integer_helper (subseq str 1)))
		(t (is_integer_helper str))))

(defun is_integer_helper(str)
	"Checks the given string is integer or not recursively except negative integer."
	(cond
		((= 0 (length str)) t)
		((is_digit (char str 0)) (is_integer_helper (subseq str 1)))
		(t nil)))


(defun get_line_list (filename)
	"Returns list of lines in the given file."
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))


(defun get_word_list (lst)
	"Returns list of words."
	(cond
		((null lst) lst)
		(t (append (split_by_space (car lst)) (get_word_list (cdr lst))))))


(defun split_by_space (string)
	"Splits given string by space and return list of splitted strings."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun is_whitespace(ch)
	"Checks the given character is whitespace or not."
	(or (char= ch #\Space)
		(char= ch #\Backspace)
		(char= ch #\Tab)
		(char= ch #\Linefeed)
		(char= ch #\Page)
		(char= ch #\Return)
		(char= ch #\Rubout)))


(defun remove_whitespaces_from_str(str)
	"Removes whitespace characters from given string."
	(cond
		((= 0 (length str)) str)
		((is_whitespace (char str 0)) (remove_whitespaces_from_str (subseq str 1)))
		(t (concatenate 'string (string (char str 0)) (remove_whitespaces_from_str (subseq str 1))))))


(defun remove_whitespaces_from_lst(lst)
	"Removes whitespace characters of string in given list."
	(cond
		((null lst) nil)
		((equal "" (car lst)) (remove_whitespaces_from_lst (cdr lst)))
		(t (cons (remove_whitespaces_from_str (car lst)) (remove_whitespaces_from_lst (cdr lst))))))


;; path of the g++ language source file is given as parameter to lexer function
(lexer "example_program.gpp")

