(define (ascending? s) 
  (define (helper? pre rest)
    (if (null? rest)
      #t
      (if (> pre (car rest))
        #f
        (helper? (car rest) (cdr rest)))))
  (if (null? s)
    #t
    (helper? (car s) (cdr s))))
  
    
;(define (my-filter pred s) 
 ; (if (null? s)
  ;  nil
   ; (if (pred (car s))
    ;  (cons (car s) (my-filter pred (cdr s)))
     ; (my-filter pred (cdr s)))))

(define (my-filter pred s)
  (define a (filter-reverse pred s nil))
  (reverse-iter a nil))
;;;
(define (filter-reverse pred s m)
  (if (null? s)
    m
    (if (pred (car s))
      (filter-reverse pred (cdr s) (cons (car s) m))
      (filter-reverse pred (cdr s) m))))

(define (reverse-iter s r)
  (if (null? s)
    r
    (reverse-iter (cdr s) (cons (car s) r))))
;;;

(define (interleave lst1 lst2) 
  (if (null? lst1) 
    lst2
    (cons (car lst1) (interleave lst2 (cdr lst1)))))

(define (no-repeats s)
  (if (null? s) 
    s
    (if (repeat (cdr s) (car s))
      (no-repeats (cdr s))
      (cons (car s) (no-repeats (cdr s))))))

(define (repeat s n) 
  (if (null? s)
    #f
    (if (= n (car s))
      #t
      (repeat (cdr s) n))))