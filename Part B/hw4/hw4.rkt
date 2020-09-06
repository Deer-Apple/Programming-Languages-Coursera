
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (< n 1)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) (cons s (lambda () (f (if (equal? s "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (let ([pr (s)]) (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (letrec ([x (list-nth-mod xs n)]
                                   [y (list-nth-mod ys n)])
                            (cons (cons x y) (lambda () (f (+ 1 n))))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (ind)
                (if (>= ind (vector-length vec))
                    #f
                    (let ([pr (vector-ref vec ind)])
                      (if (and (pair? pr) (equal? v (car pr)))
                          pr
                          (f (+ 1 ind))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0]
           [next-pos (lambda (current-ind) (remainder (+ 1 current-ind) n))]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                       ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo pos new-ans)
                              (set! pos (next-pos pos))
                              new-ans)
                            #f)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([v1 e1]
              [f (lambda (v2)
                   (if (>= v2 v1) #t (f e2)))])
       (f e2))]))