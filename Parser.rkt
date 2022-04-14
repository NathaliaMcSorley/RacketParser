#lang racket 
 
;SCANNER
(define (scanner charlist symbol memory location)
  (cond [(and (null? charlist) (not (equal? symbol 'end!))) (error "Syntax error: unexpected end of file")]
        [(null? charlist) '()] 
        [(equal? (car charlist) #\return) (scanner (cdr charlist) symbol memory location)] 
        [else (let ((token (tokenizer (car charlist) symbol memory location))) 
                (append (car token) (scanner (cdr charlist) (cadr token) (caddr token) (cadddr token))))])) 

(define (tokenizer cur_char symbol memory location)  
  (cond [(and
          (or (equal? symbol 'void) (equal? symbol 'main) (equal? symbol 'end!))
          (or (equal? cur_char #\space) (equal? cur_char #\tab)))
         (list '(skip) symbol memory (list (+ (car location) 1) (cadr location)))]
        [(and
          (or (equal? symbol 'void) (equal? symbol 'main) (equal? symbol 'end!))
          (equal? cur_char #\newline))
         (list '(skip) symbol memory (list 1 (+ (cadr location) 1)))]
        
        [(and
          (equal? symbol 'main)
          (or
           (equal? cur_char #\()
           (equal? cur_char #\))
           (equal? cur_char #\;)
           (equal? cur_char #\=)))
         (list (list (string->symbol (string cur_char))) symbol memory (list (+ (car location) 1) (cadr location)))]

        [(and
          (equal? symbol 'main)
          (or
           (equal? cur_char #\+)
           (equal? cur_char #\-)))
         (list '(skip) 'numsign? cur_char (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'numsign?)
          (equal? cur_char #\space))
         (list (list (string->symbol (string memory))) 'main '() (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'numsign?)
          (char-numeric? cur_char))
         (list '(skip) 'expect-a-number '() (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'expect-a-number)
          (char-numeric? cur_char))
         (list '(skip) 'number (list #f (string cur_char)) (list (+ (car location) 1) (cadr location)))]
        [(equal? symbol 'expect-a-number)
         (error (string-append "Syntax error: expected a number (line " (number->string (cadr location)) ", char " (number->string (car location)) ")"))]

        [(and
          (equal? symbol 'main)
          (char-numeric? cur_char))
         (list '(skip) 'number (list #f (string cur_char)) (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'number)
          (char-numeric? cur_char))
         (list '(skip) 'number (list (car memory) (string-append (cadr memory) (string cur_char))) (list (+ (car location) 1) (cadr location)))]   
        [(and
          (equal? symbol 'number)
          (equal? cur_char #\space))
         (list '(number) 'main '() (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'number)
          (equal? cur_char #\;))
         (list '(number |;|) 'main '() (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'number)
          (equal? cur_char #\)))
         (list '(number |)|) 'main '() (list (+ (car location) 1) (cadr location)))]

        [(and
          (equal? symbol 'main)
          (char-alphabetic? cur_char))
         (list '(skip) 'word (string cur_char) (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'word)
          (char-alphabetic? cur_char))
         (list '(skip) 'word (string-append memory (string cur_char)) (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'word)
          (equal? cur_char #\space)
          (or (equal? memory "write") (equal? memory "read") (equal? memory "if")))
         (list (list (string->symbol memory)) 'main '() (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'word)
          (equal? cur_char #\space))
         (list '(id) 'main '() (list (+ (car location) 1) (cadr location)))] 
        [(and
          (equal? symbol 'word)
          (equal? cur_char #\;))
         (list '(id |;|) 'main '() (list (+ (car location) 1) (cadr location)))]
        [(and 
          (equal? symbol 'word)
          (equal? cur_char #\)))
         (list '(id |)|) 'main '() (list (+ (car location) 1) (cadr location)))]

        ;Program start with {
        [(and
          (equal? symbol 'void)
          (equal? cur_char #\{))
         (list '(skip) 'main memory (list (+ (car location) 1) (cadr location)))]
        ;Program end with }
        [(and
          (equal? symbol 'main)
          (equal? cur_char #\}))
         (list '(skip) 'ending memory (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'ending)
          (equal? cur_char #\space))
         (list '(skip) 'ending memory (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'ending)
          (equal? cur_char #\newline))
         (list '(skip) 'ending memory (list 1 (+ (cadr location) 1)))]
        ;End of file
        [(and
          (equal? symbol 'ending)
          (equal? cur_char #\$))
         (list '(skip) 'end memory (list (+ (car location) 1) (cadr location)))]
        [(and
          (equal? symbol 'end)
          (equal? cur_char #\$))
         (list '(skip) 'end! memory (list (+ (car location) 1) (cadr location)))]
        [(equal? symbol 'end!)
         (list '(skip) 'end! memory (list (+ (car location) 1) (cadr location)))]
        ;Else show error
        [else (error (string-append "Syntax error (line " (number->string (cadr location)) ", char " (number->string (car location)) ")"))]
        ))

(define (skip lst)
  (cond [(null? lst) lst]
        [(equal? 'skip (car lst)) (skip (cdr lst))]
        [else (cons (car lst) (skip (cdr lst)))]))
 
;PARSER 
(define expected-stack '(stmt_list))
 
(define (push x) 
  (set! expected-stack (append expected-stack (list x))))

(define (pop) 
  (define reversedStack (reverse expected-stack))
  (define result (car reversedStack))
    (set! expected-stack (reverse (cdr reversedStack)))
     result)

(define (match input_token)
  (let ((a (pop)))
    (if (equal? input_token a)
      input_token
      (begin (push a) #f))))

(define (parse_token input_token)
  (if (null? expected-stack)
      (error (string-append "Parse error: unexpected '" (symbol->string input_token) "'"))
      (cond
        [(equal? (car (reverse expected-stack)) 'stmt_list)
             (begin (pop) (push 'stmt_list) (push 'stmt) (parse_token input_token))]
        [(and
              (equal? (car (reverse expected-stack)) 'stmt)
              (or (equal? input_token 'id)))
             (begin (pop) (push '|;|) (push 'expr) (push '=))]
        [(and
              (equal? (car (reverse expected-stack)) 'stmt)
              (or (equal? input_token 'if)))
             (begin (pop) (push 'stmt) (push '|)|) (push 'expr) (push '|(|))]
        [(and
              (equal? (car (reverse expected-stack)) 'stmt)
              (or (equal? input_token 'read)))
             (begin (pop) (push '|;|) (push 'id))]
        [(and
              (equal? (car (reverse expected-stack)) 'stmt)
              (or (equal? input_token 'write)))
             (begin (pop) (push '|;|) (push 'expr))]
        [(and
              (equal? (car (reverse expected-stack)) 'expr)
              (or (equal? input_token 'id) (equal? input_token 'number)))
             (begin (pop) (push 'etail))]
        [(and
              (equal? (car (reverse expected-stack)) 'etail)
              (or (equal? input_token '+) (equal? input_token '-)))
             (begin (pop) (push 'expr))]
        [(equal? (car (reverse expected-stack)) 'etail) (begin (pop) (parse_token input_token))]
        [(not (match input_token))
             (error (string-append "Parse error: unexpected '" (symbol->string input_token) "'"))])))

(define (read filepointer) 
  (let ((file (read-char filepointer)))
    (cond [(eof-object? file) '()]
          [else (cons file (read filepointer))])))

(define (parse-rec lst)
  (cond [(and (null? lst) (null? (begin (pop) expected-stack))) "Accept"]
        [(null? lst) (error "Unexpected end of program")]
        [else (begin (parse_token (car lst)) (parse-rec (cdr lst)))]))

(define (parse filename)
  (begin (set! expected-stack '(stmt_list))
         (parse-rec (skip (scanner (read (open-input-file filename)) 'void '() '(1 1))))))