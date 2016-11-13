#lang eopl
(require "syntax.rkt")
(require "ABSTRACTTYPES.rkt")
(require "checker.rkt")

;;; Example programs

; module m1
;  interface 
;    [a : int
;    b : int]
;  body
;    [a = 33
;     c = -(a,1)
;     b = -(c,a)]
; let a = 10
;  in -(-(from m1 take a, from m1 take b), a)

(define p1
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'a (int-type)) (val-decl 'b (int-type))))
          (defns-module-body (list (val-defn 'a (const-exp 33)) 
                                   (val-defn 'c (diff-exp (var-exp 'a) (const-exp 1)))
                                   (val-defn 'b (diff-exp (var-exp 'c) (var-exp 'a)))))))
   (let-exp 'a
            (const-exp 10)
            (diff-exp (diff-exp (qualified-var-exp 'm1 'a) (qualified-var-exp 'm1 'b)) (var-exp 'a)))))

; NOTE: TYPE ERROR!
; module m1 
;  interface 
;   [u : bool]
;  body 
;   [u = 33]
; 44

(define p2
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (bool-type))))
          (defns-module-body (list (val-defn 'u (const-exp 33)) ))))
   (const-exp 44)))

; NOTE: TYPE ERROR: undefined v
; module m1 
;  interface 
;   [u : int
;    v : int]
;  body 
;   [u = 33]
; 44

(define p3
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) (val-decl 'v (int-type))))
          (defns-module-body (list (val-defn 'u (const-exp 33)) ))))
   (const-exp 44)))

; NOTE: TYPE ERROR: order of defns
; module m1 
;  interface 
;   [u : int
;    v : int]
;  body 
;   [v = 33
;    u = 44]
; from m1 take u
(define p4
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) (val-decl 'v (int-type))))
          (defns-module-body (list (val-defn 'v (const-exp 33)) (val-defn 'u (const-exp 44)) ))))
   (qualified-var-exp 'm1 'u)))

; module m1 
;  interface 
;   [u : int] 
;  body 
;   [u = 44]
; module m2 
;  interface
;   [v : int] 
;  body 
;   [v = -(from m1 take u,11)]
;-(from m1 take u, from m2 take v)
(define p5
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) ))
          (defns-module-body (list (val-defn 'u (const-exp 44)) )))
         (a-module-definition 
          'm2
          (simple-iface (list (val-decl 'v (int-type)) ))
          (defns-module-body (list (val-defn 'v (diff-exp (qualified-var-exp 'm1 'u) (const-exp 11))) ))))
   (diff-exp (qualified-var-exp 'm1 'u) (qualified-var-exp 'm2 'v))))

; ERROR: order of modules
; module m2 
;  interface
;   [v : int] 
;  body 
;   [v = -(from m1 take u,11)]
; module m1 
;  interface 
;   [u : int] 
;  body 
;   [u = 44]
;-(from m1 take u, from m2 take v)

(define p6
  (a-program
   (list (a-module-definition 
          'm2
          (simple-iface (list (val-decl 'v (int-type)) ))
          (defns-module-body (list (val-defn 'v (diff-exp (qualified-var-exp 'm1 'u) (const-exp 11))) )))
         (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) ))
          (defns-module-body (list (val-defn 'u (const-exp 44)) )))
         )
   (diff-exp (qualified-var-exp 'm1 'u) (qualified-var-exp 'm2 'v))))

; module m1
;  interface 
;    [a : int
;    f : int -> int]
;  body
;    [a = 33
;     f = proc (x:int) x]
;  in ((from m1 take f) (from m1 take a))


(define p7
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'a (int-type)) (val-decl 'f (proc-type (int-type) (int-type)))))
          (defns-module-body (list (val-defn 'a (const-exp 33))
                                   (val-defn 'f (proc-exp 'x (int-type) (var-exp 'x)))))))
   (call-exp (qualified-var-exp 'm1 'f) (qualified-var-exp 'm1 'a))))

; module m1
;   interface
;   [ opaque t
;     z : t
;     s : (t -> t)
;     is-z? : (t -> bool) ]
;   body
;   [ type t = int
;     z = 33
;     s = proc(x:t)(x - -1)
;     is-z? = proc(x:t)zero? (x - z) ]
; proc(x:from m1 take t)((from m1 take is-z?) x)

(define p8
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'z (named-type 't)) 
                              (val-decl 's (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-z? (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'z (const-exp 33))
                              (val-defn 's (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -1))))
                              (val-defn 'is-z? (proc-exp 'x (named-type 't) (zero?-exp (diff-exp (var-exp 'x) (var-exp 'z)))))
                              ))))
   (proc-exp 'x (qualified-type 'm1 't) (call-exp (qualified-var-exp 'm1 'is-z?) (var-exp 'x)))))

;Example 8.9 page 298
(define p9
  (a-program
   (list (a-module-definition 
          'ints1
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'zero (named-type 't)) 
                              (val-decl 'succ (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'pred (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-zero (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'zero (const-exp 0))
                              (val-defn 'succ (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -5))))
                              (val-defn 'pred (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp 5))))
                              (val-defn 'is-zero (proc-exp 'x (named-type 't) (zero?-exp (var-exp 'x))))
                              ))))
   (let-exp 'z
            (qualified-var-exp 'ints1 'zero)
            (let-exp 's
                     (qualified-var-exp 'ints1 'succ)
                     (call-exp (var-exp 's) (call-exp (var-exp 's) (var-exp 'z)))))))


; Example 8.10 page 298
(define p10
  (a-program
   (list (a-module-definition 
          'ints2
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'zero (named-type 't)) 
                              (val-decl 'succ (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'pred (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-zero (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'zero (const-exp 0))
                              (val-defn 'succ (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp 3))))
                              (val-defn 'pred (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -3))))
                              (val-defn 'is-zero (proc-exp 'x (named-type 't) (zero?-exp (var-exp 'x))))
                              ))))
   (let-exp 'z
            (qualified-var-exp 'ints2 'zero)
            (let-exp 's
                     (qualified-var-exp 'ints2 'succ)
                     (call-exp (var-exp 's) (call-exp (var-exp 's) (var-exp 'z)))))))

; Example 8.11 page 299
(define p11
  (a-program
   (list (a-module-definition 
          'ints1
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'zero (named-type 't)) 
                              (val-decl 'succ (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'pred (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-zero (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'zero (const-exp 0))
                              (val-defn 'succ (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -5))))
                              (val-defn 'pred (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp 5))))
                              (val-defn 'is-zero (proc-exp 'x (named-type 't) (zero?-exp (var-exp 'x))))
                              ))))
   (let-exp 'z
            (qualified-var-exp 'ints1 'zero)
            (let-exp 's
                     (qualified-var-exp 'ints1 'succ)
                     (let-exp 'p
                     (qualified-var-exp 'ints1 'pred)
                     (let-exp 'z?
                     (qualified-var-exp 'ints1 'is-zero)
                     (letrec-exp (int-type) 'to-int 'x (qualified-type 'ints1 't)
                                 (if-exp (call-exp (var-exp 'z?) (var-exp 'x)) (const-exp 0) 
                                         (diff-exp (call-exp (var-exp 'to-int) (call-exp (var-exp 'p) (var-exp 'x))) (const-exp -1)))
                                 (call-exp (var-exp 'to-int) (call-exp (var-exp 's) (call-exp (var-exp 's) (var-exp 'z)))))
                     
                     
                      ))))))
; Example 8.12 page 299
(define p12
  (a-program
   (list (a-module-definition 
          'ints2
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'zero (named-type 't)) 
                              (val-decl 'succ (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'pred (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-zero (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'zero (const-exp 0))
                              (val-defn 'succ (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp 3))))
                              (val-defn 'pred (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -3))))
                              (val-defn 'is-zero (proc-exp 'x (named-type 't) (zero?-exp (var-exp 'x))))
                              ))))
   (let-exp 'z
            (qualified-var-exp 'ints2 'zero)
            (let-exp 's
                     (qualified-var-exp 'ints2 'succ)
                     (let-exp 'p
                     (qualified-var-exp 'ints2 'pred)
                     (let-exp 'z?
                     (qualified-var-exp 'ints2 'is-zero)
                     (letrec-exp (int-type) 'to-int 'x (qualified-type 'ints2 't)
                                 (if-exp (call-exp (var-exp 'z?) (var-exp 'x)) (const-exp 0) 
                                         (diff-exp (call-exp (var-exp 'to-int) (call-exp (var-exp 'p) (var-exp 'x))) (const-exp -1)))
                                 (call-exp (var-exp 'to-int) (call-exp (var-exp 's) (call-exp (var-exp 's) (var-exp 'z)))))
                     
                     
                      ))))))

; TYPE ERROR
(define p13
  (a-program
   (list (a-module-definition 
          'ints1
          (simple-iface (list (opaque-type-decl 't) 
                              (val-decl 'zero (named-type 't)) 
                              (val-decl 'succ (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'pred (proc-type (named-type 't) (named-type 't)))
                              (val-decl 'is-zero (proc-type (named-type 't) (bool-type)))))
          (defns-module-body (list 
                              (type-defn 't (int-type))
                              (val-defn 'zero (const-exp 0))
                              (val-defn 'succ (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp -5))))
                              (val-defn 'pred (proc-exp 'x (named-type 't) (diff-exp (var-exp 'x) (const-exp 5))))
                              (val-defn 'is-zero (proc-exp 'x (named-type 't) (zero?-exp (var-exp 'x))))
                              ))))
   (let-exp 'z
            (qualified-var-exp 'ints1 'zero)
            (diff-exp (var-exp 'z) (var-exp 'z)))))


(define (test p)
  (begin
    (display (program->string p)) 
    (display "\n")
    (display "RESULT:\n")
    (display (value-of-program p))
    (display "\nTYPE\n")
    (display (type->string (type-of-program p)))
    ))



