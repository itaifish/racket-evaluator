#lang racket

(require (only-in (file "a3.rkt") parse))

(define (eval source-code-str)
  (eval-program (parse source-code-str)))

(define (eval-program program-expr-list)
  (let ([res (eval-expr-list (second program-expr-list))])
    (last res)))

(define (eval-expr-list expr-list)
  (let* ([expr (second expr-list)]
         [optExprList (third expr-list)])
    (cons
        (eval-expr expr)
        (eval-opt-expr-list optExprList))))

(define (eval-expr expr)
    (let* ([expr-data (second expr)]
           [name (first expr-data)])
        (if (eq? name 'atom)
            (eval-atom expr-data)
            ;;'())))
            (eval-invocation expr-data))))

(define (eval-atom atom)
  (let* ([atom-data (second atom)]
           [name (first atom-data)])
        (cond [(eq? name 'STRING) (eval-string atom-data)]
              [(eq? name 'number) (eval-number atom-data)]
              [else (eval-name atom-data)])))

(define (eval-string string)
  (second string))

(define (eval-number number)
  (second (second number)))

(define and-special
  (λ ands
    (cond
      [(eq? (length ands) 0) #t]
      [(eq? (length ands) 1) (first ands)]
      [else (and (first ands) (apply and-special (rest ands)))])))

(define or-special
  (λ ors
    (cond
      [(eq? (length ors) 0) #f]
      [(eq? (length ors) 1) (first ors)]
      [else (or (first ors) (apply or-special (rest ors)))])))

;;Names hash
(define names (hash
                   '+ +
                   '- -
                   '* *
                   '/ /
                   'string-append string-append
                   'string<? string<?
                   'string=? string=?
                   'not not
                   '= =
                   '< <
                   'and and-special
                   'or or-special))

(define (eval-name name)
  (hash-ref names (second name)))

(define (eval-invocation invocation)
  (let* ([exprRes (eval-expr-list (third invocation))]
         [rator (first exprRes)]
         [rand (rest exprRes)])
    (apply rator rand)))

(define (eval-opt-expr-list optExprList)
  (if (= (length optExprList) 1)
      '()
      (eval-expr-list (second optExprList))))