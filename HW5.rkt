;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      ((eq? (car no-code) 'call)
       (append (list 'call-exp)
             (map no-parser (cdr no-code)))))))



(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x y) (do-mathy-stuff + x y)) 5 7))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (append (two-lister (cdr (cadr (cadr parsed-no-code))) (get-variables (cddr parsed-no-code))) env))))))

(define two-lister
  (lambda (lst1 lst2)
    (if (null? lst1) '()
        (cons (list (car lst1) (car lst2)) (two-lister (cdr lst1) (cdr lst2))))))

(define get-variables
  (lambda (lst)
    (if (null? lst) '()
        (cons (cadr (car lst)) (get-variables (cdr lst))))))

(run-parsed-code (no-parser sample-no-code) env)

(define s (list 'call-exp (list 'func-exp (list 'params 'x 'y) (list 'body (list 'var-exp 'x))) (list 'num-lit-exp 9) (list 'num-lit-exp 2)))
(define s2 (list 'call-exp (list 'func-exp (list 'params) (list 'body (list 'var-exp 'x)))))

(no-parser sample-no-code)
