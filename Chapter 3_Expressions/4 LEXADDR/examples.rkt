#lang eopl
(require "syntax.rkt")
(require "LEXADDR.rkt")
(require "Contour-diagrams.rkt")

;;; Example programs

; 2
(define p1 (a-program (const-exp 2)))

; 2 - 4
(define p2 (a-program (diff-exp (const-exp 2) (const-exp 4))))

; 10 - (2 - 4)
(define p3 
  (a-program (diff-exp (const-exp 10) 
                       (diff-exp (const-exp 2) (const-exp 4)))))

; x
(define p4
  (a-program (var-exp 'x)))

; x - 4
(define p5
  (a-program (diff-exp (var-exp 'x) (const-exp 4))))

; let x = 12 in x - 4
(define p6
  (a-program (let-exp 'x 
                      (const-exp 12) 
                      (diff-exp (var-exp 'x) (const-exp 4)))))
; let x = 0 in let x = 2 in x
(define p7
  (a-program (let-exp 'x (const-exp 0)
                      (let-exp 'x (const-exp 2)
                               (var-exp 'x)))))
; let x = 0 in let y = 2 in y - x
(define p77
  (a-program (let-exp 'x (const-exp 0)
                      (let-exp 'y (const-exp 2)
                               (diff-exp (var-exp 'x) (var-exp 'y))))))

; let f = proc(x) (x - 11) in (f (f 77))
(define p8
  (a-program (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 77))))))

;( proc (f) (f (f 77)) 
;  proc (x) (x - 11) )
(define p9
  (a-program (call-exp (proc-exp 'f (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 77))))
                       (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11))))))

; let x = 200
; in let f = proc(z) (z - x)
;    in let x = 100
;       in let g = proc (z) (z - x)
;          in (f 1) - (g 1)
(define p10
  (a-program (let-exp 'x (const-exp 200)
                      (let-exp 'f (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
                               (let-exp 'x (const-exp 100)
                                        (let-exp 'g (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
                                                 (diff-exp (call-exp (var-exp 'f) (const-exp 1))
                                                           (call-exp (var-exp 'g) (const-exp 1)))))))))

; let f = proc(w) 
;           let x = 100
;           in proc (z) (z - x)
; in let x = 200 in ((f 0) 0)
(define p11
  (a-program 
   (let-exp 'f 
            (proc-exp 'w 
                      (let-exp 'x 
                               (const-exp 100) 
                               (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)) )))
            (let-exp 'x 
                     (const-exp 200)
                     (call-exp (call-exp (var-exp 'f) (const-exp 0)) (const-exp 0))))))


;let f = proc(x) 10 in
;  let f = proc (x) if zero? x then 1 else (f (x - 1))
;     in (f 3)
(define p12
  (a-program 
   (let-exp 'f 
            (proc-exp 'x 
                      (const-exp 10))
            (let-exp 'f 
                     (proc-exp 'x 
                      (if-exp (zero?-exp (var-exp 'x))
                              (const-exp 1)
                              (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1)))))
                     (call-exp (var-exp 'f) (const-exp 3))
                     ))))

; let x=3 in let y=4 in (let x = y-5 in x-y) - x
(define p13
  (a-program 
   (let-exp 'x 
            (const-exp 3)
            (let-exp 'y
                     (const-exp 4)
                     (diff-exp
                     (let-exp 'x
                              (diff-exp (var-exp 'y) (const-exp 5))
                              (diff-exp (var-exp 'x) (var-exp 'y)))
                     (var-exp 'x))))))

; let x= 37 
; in proc (y)
;     let z = y - x
;     in x - y
(define p14
  (a-program 
   (let-exp 'x
            (const-exp 37)
            (proc-exp 'y 
                      (let-exp 'z 
                     (diff-exp (var-exp 'y) (var-exp 'x))
                     (diff-exp (var-exp 'x) (var-exp 'y)))))))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (display"\n ------ TRANSLATES TO: --------------\n\n")
    (display (program->string (translation-of-program p)))
    (display "\n")
    (display"\n ------ EVALUATES TO: --------------\n\n")
    (display (value-of-program (translation-of-program p)))
    (display"\n ------ CONTOUR DIAGRAM: --------------\n\n")
    (list (da p) (da (translation-of-program p)))
    ))



