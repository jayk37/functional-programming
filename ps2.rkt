#lang plai-typed

(require "ps2-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.

;; PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRR FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
;; PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRR FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
;; PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRR FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (valC (numV (s-exp->number s)))]
    [(s-exp-boolean? s) (valC (boolV (s-exp->boolean s)))]
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
            [(list) (listC (map parse (rest l)))]
            [(cons) (consC (parse (second l)) (parse (third l)))]
            [(first) (firstC (parse (second l)))]
            [(rest) (restC (parse (second l)))]
            [(natrec)
             (let* ([e1 (parse (second l))]
                    [e2 (parse (third l))]
                    [xy-list (s-exp->list (fourth l))]
                    [x (s-exp->symbol (first xy-list))]
                    [y (s-exp->symbol (second xy-list))]
                    [e3 (parse (third xy-list))])
               (natrecC e1 e2 x y e3))]
            [(listrec)
             (let* ([e1 (parse (second l))]
                    [e2 (parse (third l))]
                    [hdres-list (s-exp->list (fourth l))]
                    [hd (s-exp->symbol (first hdres-list))]
                    [rest (s-exp->symbol (second hdres-list))]
                    [res (s-exp->symbol (third hdres-list))]
                    [e3 (parse (fourth hdres-list))])
               (listrecC e1 e2 hd rest res e3))]
            [(let)
             (let* ([bindings-list (s-exp->list (second l))]
                    [bindings (map (lambda (binding)
                                     (let ([binding-list (s-exp->list binding)])
                                       (pair (s-exp->symbol (first binding-list))
                                             (parse (second binding-list)))))
                                   bindings-list)]
                    [body (parse (third l))])
               (letC bindings body))]
            [(let*)
             (let* ([bindings-list (s-exp->list (second l))]
                    [bindings (map (lambda (binding)
                                     (let ([binding-list (s-exp->list binding)])
                                       (pair (s-exp->symbol (first binding-list))
                                             (parse (second binding-list)))))
                                   bindings-list)]
                    [body (parse (third l))])
               (let*C bindings body))]
            [(unpack)
             (let ([vars (map s-exp->symbol (s-exp->list (second l)))]
                    [e1 (parse (third l))]                             
                    [e2 (parse (fourth l))])                           
               (unpackC vars e1 e2))]
            [else (error 'parse "Unknown Expression Type!")])]))]))

;; EVALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
;; EVALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
;; EVALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL FUNCTIONNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No Binding Found!")]))


(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [valC (v) v]
    [plusC (e1 e2)
           (let ([v1 (eval-env env e1)]
                 [v2 (eval-env env e2)])
             (numV (+ (numV-n v1) (numV-n v2))))]
    [timesC (e1 e2)
            (let ([v1 (eval-env env e1)]
                  [v2 (eval-env env e2)])
              (numV (* (numV-n v1) (numV-n v2))))]
    [idC (x) (lookup x env)]
    [equal?C (e1 e2)
             (let ([v1 (eval-env env e1)]
                   [v2 (eval-env env e2)])
               (boolV (equal? v1 v2)))]
    [ifC (guard e1 e2)
         (let ([v (eval-env env guard)])
           (if (boolV-b v) (eval-env env e1) (eval-env env e2)))]
    [listC (es)
           (listV (map (lambda (e) (eval-env env e)) es))]
    [consC (e1 e2)
           (let ([v1 (eval-env env e1)]
                 [v2 (listV-vs (eval-env env e2))])
             (listV (cons v1 v2)))]
    [firstC (e)
            (let ([list-val (listV-vs (eval-env env e))])
              (first list-val))]
    [restC (e)
           (let ([list-val (listV-vs (eval-env env e))])
                 (listV (rest list-val)))]

    [natrecC (e1 e2 x y e3)
             (let ([n (numV-n (eval-env env e1))])
               (if (zero? n)
                   (eval-env env e2)
                   (let ([v-rec (eval-env env (natrecC (valC (numV (- n 1))) e2 x y e3))])
                     (eval-env (extend-env (bind y v-rec) (extend-env (bind x (numV (- n 1))) env)) e3))))]

    [listrecC (e1 e2 hd tail res e3)
              (let ([list-val (listV-vs (eval-env env e1))])
                (if (empty? list-val)
                    (eval-env env e2)
                    (let ([v-rec (eval-env env (listrecC (valC (listV (rest list-val))) e2 hd tail res e3))])
                      (eval-env (extend-env (bind res v-rec) (extend-env (bind tail (listV (rest list-val))) (extend-env (bind hd (first list-val)) env))) e3))))]

    [letC (bindings body)
          (let* ([evaluated-bindings
                  (map (lambda (binding)
                         (let ([name (fst binding)]
                               [expr (snd binding)])
                           (pair name (eval-env env expr))))
                       bindings)]
                 [extended-env
                  (foldl (lambda (binding env)
                           (let ([name (fst binding)]
                                 [value (snd binding)])
                             (extend-env (bind name value) env)))
                         env
                         evaluated-bindings)])
            (eval-env extended-env body))]

    [let*C (bindings body)
           (letrec ([eval-sequential (lambda (bindings env)
                                       (if (empty? bindings)
                                           env
                                           (let ([name (fst (first bindings))]
                                                 [expr (snd (first bindings))])
                                             (let ([value (eval-env env expr)])
                                               (eval-sequential (rest bindings)
                                                                (extend-env (bind name value) env))))))])
             (eval-env (eval-sequential bindings env) body))]

    [unpackC (vars e1 e2)
             (let ([list-values (listV-vs (eval-env env e1))])
               (letrec ([bind-vars (lambda (vars values env)
                          (if (empty? vars)
                              env
                              (bind-vars (rest vars)
                                         (rest values)
                                         (extend-env (bind (first vars) (first values)) env))))])
      (let ([extended-env (bind-vars vars list-values env)])
        (eval-env extended-env e2))))]))
    
(define (eval (e : Expr)) : Value
  (eval-env empty-env e))


