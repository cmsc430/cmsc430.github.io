#lang racket
(provide make-heap-diagram)
(require pict)
(require pict/code)

(define pi 3.14)


(define n 40)

(define (make-str-cells s)
  (hc-append 
   (foldl (λ (c p) 
            (hc-append p 
                       (cc-superimpose (rectangle (/ n 2) n) ((current-code-tt) (string c)))))
          (cc-superimpose (rectangle n n) (code #,(string-length s)))
          (string->list s))
   (if (even? (string-length s))
       (rectangle 0 n)
       (cc-superimpose (rectangle (/ n 2) n) (code 0)))))                      

(define (make-imm-cell i)
  (cc-superimpose
   (code #,i)
   (rectangle n n)))

(define (make-cons-cell)
  (cb-superimpose (rectangle n n)
                  (code cons)))

(define (make-str-cell)
  (cb-superimpose (rectangle n n)
                  (code str)))

(define (make-box-cell)
  (cb-superimpose (rectangle n n)
                  (code box)))

(define (make-vect-cell)
  (cb-superimpose (rectangle n n)
                  (code vect)))



(define (fwd-pts-to a b p)
  (pin-arrow-line 7 p
                  a cc-find
                  b lt-find
                  #:start-angle (/ pi 2)
                  #:end-angle (- (/ pi 2))
                  #:start-pull 1/4
                  #:end-pull 1/2))


#|
(define rax (make-cons-cell))
(define m
  (let ((a (make-imm-cell 1))
        (b (make-cons-cell))
        (c (make-imm-cell 2))
        (d (make-cons-cell))
        (e (make-imm-cell 3))
        (f (make-imm-cell ''())))
    (define pre
      (foldr (λ (p1 p2)
               (hc-append 0 p1 p2))
             (rectangle 0 n)
             (list a b c d e f)))
    (define heap
      (vc-append 0 (fwd-pts-to d e (fwd-pts-to b c pre))
                 (text "heap")))

    (define all
      (hc-append n (vc-append 0 rax (text "rax")) heap))

    (define q
      (fwd-pts-to rax heap all))
    
    (inset q 20)))
|#



(define (make-cell v)
  (match v
    [`(cons ,_) (make-cons-cell)]
    [`(box ,_) (make-box-cell)]
    [`(vect ,_) (make-vect-cell)]
    [`(str ,_) (make-str-cell)]
    [(? string?) (make-str-cells v)]
    [_ (make-imm-cell v)]))

(define (add-arrows spec cells p)
  ;(printf "~a~n" spec)
  (match spec
    ['() p]
    [(cons `(,_ ,(? integer? i)) s)
     
     (add-arrows s
                 cells
                 (fwd-pts-to (list-ref cells (sub1 (- (length cells) (length s))))
                             (list-ref cells i)
                             p))]
    [(cons _ s) (add-arrows s cells p)]))

(define (make-heap-diagram spec)
  (match spec
    [(cons (and `(,_ ,i) r) h)
     (define rax (make-cell r))
     (define heap (map make-cell h))
     (define heap/arrows
       (add-arrows (rest spec) heap
                   (foldr (λ (p1 p2)
                            (hc-append 0 p1 p2))
                          (rectangle 0 n)
                          heap)))

     (define heap/arrows/label
       (vc-append 10
        (vc-append 
         0
         heap/arrows
         (text "heap"))
        (text "← lower addresses, higher addresses →")))

     (define rax/label
       (vc-append 10 (vc-append 0 rax (text "rax"))
                  (text " ")))
     
     (inset
      (fwd-pts-to rax (list-ref heap i)
                  (hc-append n rax/label heap/arrows/label))
      0 (* n 2) 0 0)]))

#;
(define cons-quiz
  (list (make-heap-diagram
         '((cons 4)
           3
           '()  
           2
           (cons 0)
           1
           (cons 2)))
        (make-heap-diagram
         '((cons 0)
           1
           (cons 2)
           2
           (cons 4)
           3
           '()))
        (make-heap-diagram
         '((cons 4)
           '()
           3
           (cons 0)
           2
           (cons 2)
           1))
        (make-heap-diagram
         '((cons 0)
           (cons 2)
           1
           (cons 4)
           2
           '()
           3))
        ))



;(text "← lower addresses, higher addresses →")

#;
(make-heap-diagram
 '((cons 0)
  1
  (cons 2)
  2
  (cons 4)
  3
  '()))

#;
(make-heap-diagram
 '((cons 4)
  3
  '()  
  2
  (cons 0)
  1
  (cons 2)))




#;
(let ((a (make-imm-cell 3))
      (b (make-imm-cell ''()))
      (c (make-imm-cell 2))
      (d (make-cons-cell))
      (e (make-imm-cell 1))
      (f (make-cons-cell))
      (g (make-cocell ''())))
  (define pre
    (foldr (λ (p1 p2)
            (hc-append 0 p1 p2))
          (rectangle 0 n)
          (list a b c d e f g)))  
  (inset (fwd-pts-to f g (fwd-pts-to d e (fwd-pts-to b c pre))) 20))

