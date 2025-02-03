#lang plai-typed
(require "ps4-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps4-ast.rkt and README.md for more information.

;; Note that as in the previous problem set you probably want to implement a version
;; of eval that can return more general values and takes an environment / store as arguments.
;; Your eval-base would then be a wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be converted.
;;
;; For grading, the test cases all result in values that can be converted to base values.

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (f : (Value -> Value))]
  [boxV (b : (boxof Value))]
  [objectV (env : Env) (methods : (listof MethodDecl)) (fields : (listof (symbol * Value))) (parent : (optionof Value))]
  )

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
    [else (error 'lookup "No binding found")]))

(define (argument-declaration (values : (listof Value)) (names : (listof symbol))) : Env
  (letrec ([update-env
            (lambda (names values env)
              (cond
                [(empty? names) env]
                [else
                 (let ([current-binding (bind (first names) (first values))])
                   (update-env (rest names) (rest values) (extend-env current-binding env)))]))])
    (update-env names values empty-env)))

(define (retrieve-declaration (query : symbol) (dictionary : (listof MethodDecl))) : MethodDecl
  (cond
    [(cons? dictionary)
     (let ([current-method (first dictionary)])
       (if (equal? (method-decl-name current-method) query)
           current-method
           (retrieve-declaration query (rest dictionary))))]
    [else (error 'retrieve-declaration "Reached Method List End!")]))

(define (run-query (query : symbol) (dictionary : (listof MethodDecl))) : symbol
  (cond
    [(cons? dictionary)
     (let ([current-name (method-decl-name (first dictionary))])
       (if (equal? query current-name)
           'MATCH
           (run-query query (rest dictionary))))]
    [else 'NO_MATCH]))

(define (traverse-inheritance-tree (query : symbol) (current-object : Value)) : Value
  (cond
    [(equal? 'MATCH (run-query query (objectV-methods current-object)))
     current-object]
    [else
     (traverse-inheritance-tree query (some-v (objectV-parent current-object)))]))

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
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(object)
             (let* ([fields-list (s-exp->list (second l))]
                    [fields (map (lambda (field)
                                   (let ([pair-data (s-exp->list field)])
                                     (pair (s-exp->symbol (first pair-data))
                                           (parse (second pair-data)))))
                                 fields-list)]
                    [methods-list (s-exp->list (third l))]
                    [methods (map (lambda (method)
                                    (let ([decl (s-exp->list method)])
                                      (method-decl (s-exp->symbol (first decl))
                                                   (map s-exp->symbol (s-exp->list (second decl)))
                                                   (parse (third decl)))))
                                  methods-list)])
               (objectC (none) fields methods))]
            [(object-del)
             (let* ([delegate-expr (parse (second l))]
                    [fields-list (s-exp->list (third l))]
                    [fields (map (lambda (field)
                                   (let ([pair-data (s-exp->list field)])
                                     (pair (s-exp->symbol (first pair-data))
                                           (parse (second pair-data)))))
                                 fields-list)]
                    [methods-list (s-exp->list (fourth l))]
                    [methods (map (lambda (method)
                                    (let ([decl (s-exp->list method)])
                                      (method-decl (s-exp->symbol (first decl))
                                                   (map s-exp->symbol (s-exp->list (second decl)))
                                                   (parse (third decl)))))
                                  methods-list)])
               (objectC (some delegate-expr) fields methods))]
            [(msg)
             (let* ([method (s-exp->symbol (third l))]
                    [object (parse (second l))]
                    [args (map parse (rest (rest (rest l))))])
               (msgC object method args))]
            [(get-field)
             (let ([field-name (s-exp->symbol (second l))])
               (get-fieldC field-name))]
            [(set-field!)
             (let* ([field-name (s-exp->symbol (second l))]
                    [expr (parse (third l))])
               (set-field!C field-name expr))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
         ))]))

(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    [equal?C (e1 e2) (boolV (equal? (eval-env env e1) (eval-env env e2)))]
    [ifC (guard e1 e2) (if (boolV-b (eval-env env guard)) (eval-env env e1) (eval-env env e2))]
    [letC (x e1 e2)
          (let ([v1 (eval-env env e1)])
            (eval-env (extend-env (bind x v1) env) e2))
          ]
    [lambdaC (x e) (closV (lambda (arg-value) (eval-env (extend-env (bind x arg-value) env) e)))]
    [appC (e1 e2)
          (let [(v1 (eval-env env e1))
                (v2 (eval-env env e2))]
            ((closV-f v1) v2))]
    [idC (x) (lookup x env)]
    [beginC (es)
            (let ([eval-each (lambda (ex)
                          (eval-env env ex))])
              (first (reverse (map eval-each es))))]

    [objectC (delegate fields methods)
             (let* ([new-pairing (lambda (field-pair)
                               (let* ([field-name (fst field-pair)]
                                     [field-expression (snd field-pair)]
                                     [expression-value (eval-env env field-expression)])
                                 (pair field-name (boxV (box expression-value)))))]
                    [parent (if (none? delegate)
                                (none)
                                (some (eval-env env (some-v delegate))))])
               (objectV env methods (map new-pairing fields) parent))]

    [msgC (o method args)
          (let* ([child-object (eval-env env o)]
                 [self-pointer (bind 'self child-object)]
                 [parent-object (traverse-inheritance-tree method child-object)]
                 [field-update-env (map (lambda (field-pair)
                                   (bind (fst field-pair) (snd field-pair)))
                                 (objectV-fields parent-object))]
                 [method-declaration
                  (retrieve-declaration method (objectV-methods parent-object))]
                 [arguments-update-env (argument-declaration
                                        (map (lambda (argument-expression)
                                               (eval-env env argument-expression)) args)
                                        (rest (method-decl-args method-declaration)))]
                 [final-update (foldl cons
                                (objectV-env child-object)
                                (cons self-pointer
                                      (append arguments-update-env field-update-env)))])
            (eval-env final-update (method-decl-body method-declaration)))]
    
    [get-fieldC (name)
                (let* ([value (lookup name env)]           
                       [boxed-value (boxV-b value)]        
                       [unboxed-value (unbox boxed-value)]) 
                  unboxed-value)]

    [set-field!C (name e)
                 (let* ([evaluated-value (eval-env env e)]      
                        [field-value (lookup name env)]         
                        [field-box (boxV-b field-value)])       
                   (begin
                     (set-box! field-box evaluated-value)      
                     evaluated-value))]))                        

(define (eval-base (e : Expr)) : BaseValue
  (let ([result (eval-env empty-env e)])
    (type-case Value result
      [numV (n) (numBV n)]
      [boolV (b) (boolBV b)]
      [else (error 'eval-base "Expecting a Num or Bool!")]))
  )