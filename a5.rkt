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
                   '< <))

;;
;;TODO
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