#lang plai-typed
(require "ps3-ast.rkt")
(require (typed-in racket/list [list-set : ((listof 'a) number 'a -> (listof 'a))]))
(require (typed-in racket/base [list-tail : ((listof 'a) number -> (listof 'a))]))
(require (typed-in racket/list [take : ((listof 'a) number -> (listof 'a))]))

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(pair) (pairC (parse (second l)) (parse (third l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(box) (boxC (parse (second l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(vector) (vectorC (map parse (rest l)))]
            [(vector-length) (vector-lengthC (parse (second l)))]
            [(vector-ref) (vector-refC (parse (second l)) (parse (third l)))]
            [(vector-set!) (vector-set!C (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(vector-make) (vector-makeC (parse (second l)) (parse (third l)))]
            [(subvector) (subvectorC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(transact) (transactC (parse (second l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
         ))]))

(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  [pairV (v1 : Value) (v2 : Value)]
  [vectorV (contents : (listof Value))]
  [subvectorV (subvec : Value) (refvec : Value) (ofs : number)]
  )

(define-type Binding
  [bind (name : symbol) (loc : Location)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No Location Found")]))

(define (lookup (x : symbol) (env : Env)) : Location
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-loc (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No Binding found")]))

(define-type Result
  [res (v : Value) (s : Store)])

(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))

(define (find-location (val : Value) (sto : Store)) : Location
  (cond
    [(cons? sto)
     (let ([storage (first sto)])
       (if (equal? (cell-val storage) val)
           (cell-location storage)
           (find-location val (rest sto))))]
    [else (error 'find-location "Value Not Found In The Store!")]))

(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]
    [pairC (e1 e2)
           (type-case Result (eval-env env sto e1)
             [res (v1 sto-1)
                  (type-case Result (eval-env env sto-1 e2)
                    [res (v2 sto-2)
                         (res (pairV v1 v2) sto-2)])])]
    [fstC (e)
          (type-case Result (eval-env env sto e)
            [res (v sto-1)
                 (type-case Value v
                   [pairV (v1 v2) (res v1 sto-1)]
                   [else (error 'fstC "Expected a Pair Value")])])]
    [sndC (e)
          (type-case Result (eval-env env sto e)
            [res (v sto-1)
                 (type-case Value v
                   [pairV (v1 v2) (res v2 sto-1)]
                   [else (error 'sndC "Expected a Pair Value")])])]

    [plusC (e1 e2)
           (type-case Result (eval-env env sto e1)
             [res (v1 sto-1)
                  (type-case Result (eval-env env sto-1 e2)
                    [res (v2 sto-2)
                         (res (numV (+ (numV-n v1) (numV-n v2))) sto-2)])])]

    [timesC (e1 e2)
            (type-case Result (eval-env env sto e1)
              [res (v1 sto-1)
                   (type-case Result (eval-env env sto-1 e2)
                     [res (v2 sto-2)
                          (res (numV (* (numV-n v1) (numV-n v2))) sto-2)])])]

    [equal?C (e1 e2)
             (type-case Result (eval-env env sto e1)
               [res (v1 sto-1)
                    (type-case Result (eval-env env sto-1 e2)
                      [res (v2 sto-2)
                           (res (boolV (equal? v1 v2)) sto-2)])])]
    
    [ifC (guard e1 e2)
         (type-case Result (eval-env env sto guard)
           [res (v sto-1)
                (if (boolV-b v)
                    (eval-env env sto-1 e1)
                    (eval-env env sto-1 e2))])]

    [appC (e1 e2)
          (type-case Result (eval-env env sto e1)
            [res (v1 sto-1)
                 (type-case Result (eval-env env sto-1 e2)
                   [res (v2 sto-2)
                        (let ([l (new-loc)])
                          (eval-env
                           (extend-env (bind (closV-x v1) l) (closV-env v1))
                           (override-store (cell l v2) sto-2)
                           (closV-e v1)))])])]
    
    [lambdaC (x e)
             (res (closV env x e) sto)]
    
    [letC (x e1 e2)
          (eval-env env sto (appC (lambdaC x e2) e1))]


    [idC (x) (res (fetch (lookup x env) sto) sto)]

    [boxC (a) (type-case Result (eval-env env sto a)
                (res (v sto-1)
                     (let [(l (new-loc))]
                       (res (boxV l) (override-store (cell l v) sto-1)))))]

    [unboxC (a) (type-case Result (eval-env env sto a)
                  (res (v sto-1)
                       (res (fetch (boxV-l v) sto-1) sto-1)))]

    [setboxC (e1 e2)
             (type-case Result (eval-env env sto e1)
               (res (v1 sto-1)
                    (type-case Result (eval-env env sto-1 e2)
                      (res (v2 sto-2)
                           (res v2 (override-store (cell (boxV-l v1)
                                                         v2)
                                                   sto-2))))))]
    [beginC (es)
            (letrec ([eval-seq (lambda (exps env sto)
                                 (cond
                                   [(empty? exps)
                                    (error 'beginC "Begin Requires At Least One Expression!")]
                                   [(empty? (rest exps))
                                    (eval-env env sto (first exps))]
                                   [else
                                    (type-case Result (eval-env env sto (first exps))
                                      [res (v sto-1)
                                           (eval-seq (rest exps) env sto-1)])]))])
              (eval-seq es env sto))]

    [vectorC (es)
             (letrec ([eval-elements (lambda (expressions env sto acc)
                                       (cond
                                         [(empty? expressions)
                                          (res (vectorV (reverse acc)) sto)]
                                         [else
                                          (type-case Result (eval-env env sto (first expressions))
                                            [res (v sto-1)
                                                 (eval-elements (rest expressions) env sto-1 (cons v acc))])]))])
               (eval-elements es env sto empty))]

    [vector-lengthC (e)
                    (type-case Result (eval-env env sto e)
                      [res (v sto-1)
                           (type-case Value v
                             [vectorV (contents) (res (numV (length contents)) sto-1)]
                             [subvectorV (subvec refvec ofs) (res (numV (length (vectorV-contents subvec))) sto-1)]
                             [else (error 'vector-lengthC "Expected a vectorV Value")])])]

    [vector-refC (e1 e2)
                 (type-case Result (eval-env env sto e1)
                   [res (v1 sto-1)
                        (type-case Value v1
                          [vectorV (contents)
                                   (type-case Result (eval-env env sto-1 e2)
                                     [res (v2 sto-2)
                                          (let ([index (numV-n v2)])
                                            (if (and (<= 0 index) (< index (length contents)))
                                                (res (list-ref contents index) sto-2)
                                                (error 'vector-refC "Index Out of Bounds")))])]
                          [subvectorV (subvec refvec ofs)
                                   (type-case Result (eval-env env sto-1 e2)
                                     [res (v2 sto-2)
                                          (let ([index (numV-n v2)])
                                            (if (and (<= 0 index) (< index (length (vectorV-contents subvec))))
                                                (res (list-ref (vectorV-contents subvec) index) sto-2)
                                                (error 'vector-refC "Index Out of Bounds")))])]
                          [else (error 'vector-refC "Expected a vectorV or subVectorV Value")])])]

    [vector-makeC (e1 e2)
                  (type-case Result (eval-env env sto e1)
                    [res (v1 sto-1)
                         (let ([length (numV-n v1)])
                           (if (>= length 0)
                               (type-case Result (eval-env env sto-1 e2)
                                 [res (v2 sto-2)
                                      (letrec ([make-list (lambda (n value)
                                                            (if (zero? n)
                                                                empty
                                                                (cons value (make-list (- n 1) value))))])
                                        (let ([vector-content (make-list length v2)])
                                          (res (vectorV vector-content) sto-2)))])
                               (error 'vector-makeC "Length Must Be a Non-Negative Integer!")))])]


    [transactC (e)
               (let ([original-store sto])
                 (type-case Result (eval-env env sto e)
                   [res (v sto-1)
                        (type-case Value v
                          [pairV (v1 v2)
                                 (type-case Value v1
                                   [boolV (b)
                                          (if b
                                              (res v2 sto-1)
                                              (res v2 original-store))]
                                   [else (error 'transactC "Expected First Component of Pair to be Boolean")])]
                          [else (error 'transactC "Expected a Pair Value")])]))]

    
    [subvectorC (e offset len)
                (type-case Result (eval-env env sto e)
                    [res (v1 sto-1)
                         (type-case Result (eval-env env sto-1 offset)
                           [res (v2 sto-2)
                                (let ([offset_val (numV-n v2)])
                                  (type-case Result (eval-env env sto-2 len)
                                    [res (v3 sto-3)
                                         (let ([len_val (numV-n v3)]
                                               [location_val (find-location v1 sto-3)])
                                           (type-case Value v1
                                             [vectorV (contents)
                                                      (let ([subvec_return (subvectorV (vectorV (take (list-tail contents offset_val) len_val)) v1 offset_val)])
                                                        (res subvec_return sto-3))]
                                             [else (error 'subvectorC "Expected a Vector Post Subvector")]))]))])])]


    [vector-set!C (e1 e2 e3)
                  (type-case Result (eval-env env sto e1)
                    [res (v1 sto-1)
                         (type-case Value v1
                           [vectorV (contents)
                                    (let ([location (find-location v1 sto-1)])
                                      (type-case Result (eval-env env sto-1 e2)
                                        [res (v2 sto-2)
                                             (let ([index (numV-n v2)])
                                               (type-case Result (eval-env env sto-2 e3)
                                                 [res (v3 sto-3)
                                                      (let ([updated-contents (list-set contents index v3)])
                                                        (res v3 (override-store (cell location (vectorV updated-contents)) sto-3)))]))]))]

                           [subvectorV (subvec refvec ofs)
                                       (let ([location (find-location refvec sto-1)])
                                         (type-case Result (eval-env env sto-1 e2)
                                           [res (v2 sto-2)
                                                (let ([index (numV-n v2)])
                                                  (type-case Result (eval-env env sto-2 e3)
                                                    [res (v3 sto-3)
                                                         (let ([updated-contents (list-set (vectorV-contents refvec) (+ index ofs) v3)])
                                                           (res v3 (override-store (cell location (vectorV updated-contents)) sto-3)))]))]))]
                           
                           [else (error 'vector-set!C "Expected a vector value for e1")])])]
    

    ;[vectorC (es)
    ;         (error 'eval-env "VectorC operation not yet implemented.")]
    ;[vector-lengthC (e)
    ;                (error 'eval-env "Vector-lengthC operation not yet implemented.")]
    ;[vector-refC (e1 e2)
    ;             (error 'eval-env "Vector-refC operation not yet implemented.")]
    ;[vector-set!C (e1 e2 e3)
    ;              (error 'eval-env "Vector-SetC operation not yet implemented.")]
    ;[vector-makeC (e1 e2)
    ;              (error 'eval-env "Vector-MakeC operation not yet implemented.")]
    ;[subvectorC (e offset len)
    ;           (error 'eval-env "Subvector operation not yet implemented.")]
    ;[transactC (e)
    ;           (error 'eval-env "transactC operation not yet implemented.")]
    )
  )

(define (convert-to-base (v : Value)) : BaseValue
  (type-case Value v
    [numV (n) (numBV n)]
    [boolV (b) (boolBV b)]
    [pairV (v1 v2)
           (pairBV (convert-to-base v1) (convert-to-base v2))]
    [else (error 'convert-to-base "Cannot Convert to BaseValue")]))

(define (eval-base (e : Expr)) : BaseValue
  (let ([result (eval-env empty-env empty-store e)])
    (type-case Result result
      [res (v sto)
           (convert-to-base v)])))

;(define (eval-base (e : Expr)) : BaseValue
;  (error 'eval-base "Not yet implemented.")
;  )
