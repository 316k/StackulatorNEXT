(define (two-args-stack-fct f stack)
  (cons (f (cadr stack) (car stack)) (cddr stack)))

; Splits a list in two on the first occurence of a member of vals
(define (split-on vals lst)
  (let loop ((left '()) (right lst))
    (if (member (car right) vals)
	(cons (reverse left) right)
	(loop (cons (car right) left) (cdr right)))))

(define (split-before vals lst)
  (car (split-on vals lst)))

(define (split-after vals lst)
  (cddr (split-on vals lst)))

(define (split-on-matching-paren lst)
  (let loop ((left '()) (right lst) (parens-depth 0))
    (cond
     ((or (< parens-depth 0) (null? right))
      (raise "Unmatched parenthesis"))
     ((and (= parens-depth 0) (eq? (car right) #\)))
      (cons (reverse left) (cdr right)))
     ((eq? (car right) #\))
      (loop (cons #\) left) (cdr right) (+ parens-depth 1)))
     ((eq? (car right) #\()
      (loop (cons #\( left) (cdr right) (- parens-depth 1)))
     (else
      (loop (cons (car right) left) (cdr right) parens-depth)))))

(define (slack-bool x)
  (not (member x '(#f 0 ""))))

(define env
  ; Built-in functions
  `((cr fct . ,(lambda (stack) (newline) stack))
    (p fct . ,(lambda (stack) (print (car stack)) stack))
    (pp fct . ,(lambda (stack) (println (car stack)) stack))
    (+ fct . ,(lambda (stack) (two-args-stack-fct + stack)))
    (- fct . ,(lambda (stack) (two-args-stack-fct - stack)))
    (* fct . ,(lambda (stack) (two-args-stack-fct * stack)))
    (/ fct . ,(lambda (stack) (two-args-stack-fct / stack)))
    (> fct . ,(lambda (stack) (two-args-stack-fct > stack)))
    (>= fct . ,(lambda (stack) (two-args-stack-fct >= stack)))
    (< fct . ,(lambda (stack) (two-args-stack-fct < stack)))
    (<= fct . ,(lambda (stack) (two-args-stack-fct <= stack)))
    (dup fct . ,(lambda (stack) (cons (car stack) stack)))
    (x fct . ,(lambda (stack) (cdr stack)))
    (bye fct . ,(lambda (stack) (exit)))
    (reverse fct . ,(lambda (stack) (reverse stack)))
    ($length fct . ,(lambda (stack) (cons (length stack) stack)))
    ($clear fct . ,(lambda (stack) '()))
    (eval fct . ,(lambda (stack)
		   (if (pair? (car stack))
		       (parse (cdr stack) (car stack))
		       stack)))
    (|.| macro . ,(lambda (stack src) (cons stack (cons 'eval src))))
    ; Macros
    ($dump macro . ,(lambda (stack src) (pp stack) (cons stack src)))
    ($env macro . ,(lambda (stack src) (pp env) (cons stack src))) 
    ; 1 if (1 2 3 4) => (1 2 3 4) eval
    (|if|  macro .
     ,(lambda (stack src)
	(if (slack-bool (car stack))
	    (cons (cdr stack) (cons (car src) (cons 'eval (cdr src))))
	    (cons (cdr stack) (cdr src)))))
    ; 4 while (1 - "test" . cr) ... => 4 (1 - "test" . cr) eval while (1 - "test" . cr) ...
    (while macro .
	   ,(lambda (stack src)
	      (if (slack-bool (car stack))
		  (let ((body (car src)))
		    (cons stack (append (list body 'eval 'while body) (cdr src))))
		  (cons stack (cdr src)))))
    ; TODO : clean stuff instead of prepending everything
    (-> macro .
	,(lambda (stack src)
	   (set! env
		 (cons `(,(car src) var . ,(car stack))
		       env))
	   (cons (cdr stack) (cdr src))))))

(define (lookup key env)
  (let ((pair (assoc key env)))
    (and pair (cdr pair))))

(define (tokenize line)
  (reverse
   (let loop
       ((consumed '())
	(code line)
	(text? #f)
	(continue? #f))
     (define c (and (not (null? code)) (car code)))
     (define rest (if (null? code) #f (cdr code)))
     (cond
      ((or (null? code) (and (not text?) (eq? c #\#)))
       consumed)
      ; Appending text
      ((and text? (not (member c '(#\" #\\))))
       (loop (cons (string-append (car consumed) (string c)) (cdr consumed)) rest #t #t))
      ; Stop appending text
      ((and text? (eq? c #\"))
       (loop (cons (string-append (car consumed) (string #\")) (cdr consumed)) rest #f #f))
      ; Escaped chars
      ((and text?
	    (not (null? rest))
	    (eq? c #\\)
	    (eq? (car rest) #\"))
       (loop (cons (string-append (car consumed) #\") (cdr consumed)) (cdr rest) #t #t))
      ; Sublists
      ((eq? c #\()
       (let*
	   ((expr (split-on-matching-paren rest))
	    (sub-expr (car expr))
	    (rest (cdr expr)))
	 (loop (cons (tokenize sub-expr) consumed) rest #f #f)))
      ; Spaces separating tokens
      ((eq? c #\space)
       (loop consumed rest #f #f))
      ((not continue?)
       (loop (cons (string c) consumed) rest (eq? c #\") #t))
      (else
       (loop (cons (string-append (car consumed) (string c)) (cdr consumed)) rest text? #t))))))


(define (eval token)
  (let ((number (and (string? token) (string->number token))))
    (cond
     (number
      number)
     ((and (string? token) (eq? (string-ref token 0) #\"))
      (substring token 1 (- (string-length token) 1)))
     ((pair? token)
      (map eval token))
     (else
      (string->symbol token)))))

(define (parse stack src)
  (if (null? src)
      stack
      (let ((word (car src)) (rest (cdr src)))
	(cond
	 ((or (number? word) (string? word) (pair? word))
	  (parse (cons word stack) rest))
	 ((and (symbol? word) (lookup word env))
	  (let* ((expr (lookup word env)) (type (car expr)) (val (cdr expr)))
	    (case type
	      ((var)
	       (parse (cons val stack) rest))
	      ((fct)
	       (parse (val stack) rest))
	      ((macro)
	       (let*
		   ((expr (val stack rest))
		    (stack (car expr))
		    (src (cdr expr)))
		 (parse stack src))))))
         ; Bad symbols
	 ((symbol? word)
	  (raise (string-append "Word "(symbol->string word) " unknown")))
	 (else ; ignore everything else
	  (parse stack rest))))))

(define line-number 0)
(define (read-line!)
  (set! line-number (+ 1 line-number))
  (read-line))

(define debug #t)

(define (main stack)
  (main
   (with-exception-catcher
    (lambda (exc)
      (println (string-append
       (error-handler)
       (if (string? exc) 
	   (string-append " -- " exc)
	   "")
       " on line " (number->string line-number)))
      stack)
    (lambda ()
      (parse stack
	     (map eval (tokenize (string->list (read-line!)))))))))

; Graceful error handling
(define random (make-random-source))
(random-source-randomize! random)

(define (random-choose lst)
  (list-ref lst ((random-source-make-integers random) (length lst))))

(define (error-handler)
  (random-choose '("EWONTFIX" "ENOPE" "EFUCKOFF" "ERTFM" "EIDK" "ENOTMYPROBLEM" "EWORKSONMYMACHINE" "EASKMYBOSS" "EREBOOT" "ECANTREPRODUCE" "EDONTCARE" "ELUNCHBREAK")))

; Run !
(main '())
