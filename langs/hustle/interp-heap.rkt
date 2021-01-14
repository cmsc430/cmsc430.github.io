#lang racket
(provide interp interp-env-heap)
(require "heap.rkt"
         "env.rkt"
         "unload.rkt"
         "interp-prims-heap.rkt"
         "ast.rkt")

;; type Answer* =
;; | (cons Heap Value*)
;; | 'err

;; type Value* =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (list 'box  Address)
;; | (list 'cons Address)
;; | (list 'str Address)

;; type Heap = (Listof Value*)
;; type REnv = (Listof (List Id Value*))

;; Expr -> Answer
(define (interp e)  
  (unload (interp-env-heap e '() '())))

;; Expr REnv Heap -> Answer*
(define (interp-env-heap e r h)
  (match e
    [(Int i)  (cons h i)]
    [(Bool b) (cons h b)]
    [(Char c) (cons h c)]
    [(Str s)  (alloc-str s h)]
    [(Eof)    (cons h eof)]
    [(Empty)  (cons h '())]
    [(Var x)  (cons h (lookup r x))]
    [(Prim0 'read-byte) (cons h (read-byte))]
    [(Prim1 p e)
     (match (interp-env-heap e r h)
       ['err 'err]
       [(cons h v)
        (interp-prim1 p v h)])]
    [(Prim2 p e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v1)
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h v2)
           (interp-prim2 p v1 v2 h)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v1)
        (match (interp-env-heap e2 r h)
          ['err 'err]
          [(cons h v2)
           (match (interp-env-heap e3 r h)
             [(cons h v3)
              (interp-prim3 p v1 v2 v3 h)])])])]
    [(If p e1 e2)
     (match (interp-env-heap p r h)
       ['err 'err]
       [(cons h v)
        (if v
            (interp-env-heap e1 r h)
            (interp-env-heap e2 r h))])]
    [(Begin e1 e2)     
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h _) (interp-env-heap e2 r h)])]
    [(Let x e1 e2)
     (match (interp-env-heap e1 r h)
       ['err 'err]
       [(cons h v)
        (interp-env-heap e2 (ext r x v) h)])]))