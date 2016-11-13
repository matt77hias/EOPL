#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv 
   (bvar symbol?)
   (bval type?)
   (saved-tenv type-environment?))
  (extend-tenv-with-module
   (name symbol?)
   (interface interface?)
   (saved-tenv type-environment?))
  (extend-tenv-with-type
   (t-name symbol?)
   (t-type type?)                    ; invariant: this must always
   ; be expanded
   (saved-tenv type-environment?))
  )

;;;;;;;;;;;;;;;; procedures for looking things up tenvs ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; lookup or die

;; lookup-qualified-var-in-tenv : Sym * Sym * Tenv -> Type
;; Page: 285
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
                      (lookup-variable-name-in-decls var-name decls)) ))))

(define lookup-variable-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (variable-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'variable search-sym tenv)))))

(define lookup-module-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (module-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'module search-sym tenv)))))

(define lookup-type-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (type-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'type search-sym tenv)))))

(define lookup-qualified-type-in-tenv
  (lambda (m-name t-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
                      ;; this is not right, because it doesn't distinguish
                      ;; between type and variable declarations.  Exercise: fix
                      ;; this so that it raises an error if t-name is declared
                      ;; in a val-decl.
                      (lookup-variable-name-in-decls t-name decls))
        ))))

(define apply-tenv lookup-variable-name-in-tenv)

(define raise-tenv-lookup-failure-error
  (lambda (kind var tenv)
    (eopl:pretty-print
     (list 'tenv-lookup-failure: (list 'missing: kind var) 'in:
           tenv))
    (eopl:error 'lookup-variable-name-in-tenv)))


;; this is not right, because it doesn't distinguish
;; between type and variable declarations.  But it will do
;; for now.  Exercise: refine this do that it raises an error if
;; var-name is declared as something other than a val-decl.

(define lookup-variable-name-in-decls
  (lambda (var-name decls0)
    (let loop ((decls decls0))
      (cond
        ((null? decls)
         (raise-lookup-variable-in-decls-error! var-name decls0))
        ((eqv? var-name (decl->name (car decls)))
         (decl->type (car decls)))
        (else (loop (cdr decls)))))))

(define raise-lookup-variable-in-decls-error!
  (lambda (var-name decls)
    (eopl:pretty-print
     (list 'lookup-variable-decls-failure:
           (list 'missing-variable var-name)
           'in:
           decls))))

;;;;;;;;;;;;;;;; lookup or return #f.

;; variable-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Type)
(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv (name ty saved-tenv)
                     (if (eqv? name search-sym) 
                         ty
                         (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; module-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Iface)
(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (name m-type saved-tenv)
                                 (if (eqv? name search-sym) 
                                     m-type
                                     (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; type-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Iface)
(define type-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-type (name type saved-tenv)
                               (if (eqv? name search-sym) 
                                   type
                                   (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; assumes tenv is non-empty.  
(define tenv->saved-tenv
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv () 
                  (eopl:error 'tenv->saved-tenv
                              "tenv->saved-tenv called on empty tenv"))
      (extend-tenv (name ty saved-tenv) saved-tenv)
      (extend-tenv-with-module (name m-type saved-tenv) saved-tenv)
      (extend-tenv-with-type (name ty saved-tenv) saved-tenv)
      )))
;;;;;;;;;;;;;;;; expand-type ;;;;;;;;;;;;;;;;

;; expand-type expands a type so that it contains no type
;; abbreviations. 

;; For example, if tenv contains a declaration for a module

;;   module m1
;;    interface
;;     [abstract-type t
;;      type-abbrev u = int
;;      type-abbrev v = (t -> u)]

;; then calling expand-type on from m1 take v should return
;; (from m1 take t -> int)  

;; this relies on the invariant that every type returned by
;; lookup-type-name-in-tenv is already expanded.


;; expand-type : Type * Tenv -> ExpandedType
(define expand-type
  (lambda (ty tenv)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (expand-type arg-type tenv)
                  (expand-type result-type tenv)))
      (named-type (name)
                  (lookup-type-name-in-tenv tenv name))
      (qualified-type (m-name t-name)
                      (lookup-qualified-type-in-tenv m-name t-name tenv))
      )))


;; creates new interface with all types expanded
;; expand-iface : Sym * Iface * Tenv -> Iface
;; Page: 307
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
      (simple-iface (decls) 
                    (simple-iface
                     (expand-decls m-name decls tenv))) )))


;; like defns->decls, this creates only transparent type
;; declarations. 

;; expand-decls : Sym * Listof(Decl) * Tenv -> Listof(Decl)
;; Page: 307
(define expand-decls
  (lambda (m-name decls internal-tenv) 
    (if (null? decls) '()
        (cases declaration (car decls)
          (opaque-type-decl (t-name)
                            ;; here the expanded type is m.t
                            (let ((expanded-type (qualified-type m-name t-name)))
                              (let ((new-env (extend-tenv-with-type
                                              t-name
                                              expanded-type
                                              internal-tenv)))
                                (cons 
                                 (transparent-type-decl t-name expanded-type)
                                 (expand-decls m-name (cdr decls) new-env)))))
          (transparent-type-decl (t-name ty)
                                 (let ((expanded-type (expand-type ty internal-tenv)))
                                   (let ((new-env (extend-tenv-with-type 
                                                   t-name
                                                   expanded-type
                                                   internal-tenv)))
                                     (cons
                                      (transparent-type-decl t-name expanded-type)
                                      (expand-decls m-name (cdr decls) new-env)))))
          (val-decl (var-name ty)
                    (let ((expanded-type
                           (expand-type ty internal-tenv)))
                      (cons
                       (val-decl var-name expanded-type)
                       (expand-decls m-name (cdr decls) internal-tenv))))))))

;; check-equal-type! : Type * Type * Exp -> Unspecified
;; Page: 242
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (cond [(not (equal? ty1 ty2))
           (report-unequal-types ty1 ty2 exp)])))

;; report-unequal-types : Type * Type * Exp -> Unspecified
;; Page: 243
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; moved to check-modules.scm
;; type-of-program : Program -> Type
;; Page: 244 
;;   (define type-of-program
;;     (lambda (pgm)
;;       (cases program pgm
;;         (a-program (exp1) 
;;           (type-of exp1 (init-tenv))))))


;; type-of : Exp * Tenv -> Type
;; Page 244--246.  See also page 285.
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      
      (diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
      
      (zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
      
      (if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))
      
      (var-exp (var) (apply-tenv tenv var))
      
      ;; lookup-qualified-var-in-tenv defined on page 285.
      (qualified-var-exp (m-name var-name) 
                         (lookup-qualified-var-in-tenv m-name var-name tenv))
      
      (let-exp (var exp1 body)
               (let ((rhs-type (type-of exp1 tenv)))
                 (type-of body (extend-tenv var rhs-type tenv))))
      
      (proc-exp (bvar bvar-type body)
                (let ((expanded-bvar-type
                       (expand-type bvar-type tenv)))
                  (let ((result-type
                         (type-of body
                                  (extend-tenv
                                   bvar
                                   expanded-bvar-type
                                   tenv))))
                    (proc-type expanded-bvar-type result-type))))
      
      (call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                     (eopl:error 'type-of
                                 "Rator not a proc type:~%~s~%had rator type ~s"   
                                 rator (type-to-external-form rator-type))))))
      
      (letrec-exp (proc-result-type proc-name 
                                    bvar bvar-type 
                                    proc-body
                                    letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-name
                          (expand-type
                           (proc-type bvar-type proc-result-type)
                           tenv)
                          tenv)))
                    (let ((proc-result-type
                           (expand-type proc-result-type tenv))
                          (proc-body-type
                           (type-of proc-body
                                    (extend-tenv
                                     bvar
                                     (expand-type bvar-type tenv)
                                     tenv-for-letrec-body))))
                      (check-equal-type!
                       proc-body-type proc-result-type proc-body)
                      (type-of letrec-body tenv-for-letrec-body))))
      
      )))

(define rename-in-iface
  (lambda (m-type old new)
    (cases interface m-type
      (simple-iface (decls)
                    (simple-iface
                     (rename-in-decls decls old new))) )))

;; this isn't a map because we have let* scoping in a list of declarations
(define rename-in-decls
  (lambda (decls old new)
    (if (null? decls) '()
        (let ((decl (car decls))
              (decls (cdr decls)))
          (cases declaration decl
            (val-decl (name ty)
                      (cons
                       (val-decl name (rename-in-type ty old new))
                       (rename-in-decls decls old new)))
            (opaque-type-decl (name)
                              (cons
                               (opaque-type-decl name)
                               (if (eqv? name old)
                                   decls
                                   (rename-in-decls decls old new))))
            (transparent-type-decl (name ty)
                                   (cons
                                    (transparent-type-decl 
                                     name
                                     (rename-in-type ty old new))
                                    (if (eqv? name old)
                                        decls
                                        (rename-in-decls decls old new))))
            )))))

(define rename-in-type
  (lambda (ty old new)
    (let recur ((ty ty))
      (cases type ty 
        (named-type (id)
                    (named-type (rename-name id old new)))
        (qualified-type (m-name name)
                        (qualified-type
                         (rename-name m-name old new)
                         name))
        (proc-type (t1 t2)
                   (proc-type (recur t1) (recur t2)))
        (else ty)              ; this covers int, bool, and unknown.
        ))))

(define rename-name
  (lambda (name old new)
    (if (eqv? name old) new name)))

(define fresh-module-name
  (let ((sn 0))
    (lambda (module-name)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string module-name)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))
;; <:-iface : Iface * Iface * Tenv -> Bool
;; Page: 289
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
                    (cases interface iface2
                      (simple-iface (decls2)
                                    (<:-decls decls1 decls2 tenv)))))))

;; s1 <: s2 iff s1 has at least as much stuff as s2, and in the same
;; order.  We walk down s1 until we find a declaration that declares
;; the same name as the first component of s2.  If we run off the
;; end of s1, then we fail.  As we walk down s1, we record any type
;; bindings in the tenv

;; <:-decls : Listof(Decl) * Listof(Decl) * Tenv -> Bool
;; Page: 289, 305
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ;; if nothing in decls2, any decls1 will do
      ((null? decls2) #t)
      ;; nothing in decls1 to match, so false
      ((null? decls1) #f)
      (else
       ;; at this point we know both decls1 and decls2 are non-empty.
       (let ((name1 (decl->name (car decls1)))
             (name2 (decl->name (car decls2))))
         (if (eqv? name1 name2)
             ;; same name.  They'd better match
             (and
              (<:-decl (car decls1) (car decls2) tenv)
              (<:-decls (cdr decls1) (cdr decls2)
                        (extend-tenv-with-decl (car decls1) tenv)))
             ;; different names.  OK to skip, but record decl1 in the tenv. 
             (<:-decls (cdr decls1) decls2
                       (extend-tenv-with-decl (car decls1) tenv))))))))

;; extend-tenv-with-decl : Decl * Tenv -> Tenv
;; Page: 309
(define extend-tenv-with-decl
  (lambda (decl tenv)
    (cases declaration decl
      ;; don't need to record val declarations
      (val-decl (name ty) tenv)
      (transparent-type-decl (name ty)
                             (extend-tenv-with-type
                              name
                              (expand-type ty tenv)
                              tenv))
      (opaque-type-decl (name)
                        (extend-tenv-with-type
                         name
                         ;; the module name doesn't matter, since the only
                         ;; operation on qualified types is equal?
                         (qualified-type (fresh-module-name '%abstype) name)
                         tenv)))))

;; decl1 and decl2 are known to declare the same name.  There are
;; exactly four combinations that can succeed.

;; <:-decl : Decl * Decl * Tenv -> Bool
;; Page: 311
(define <:-decl 
  (lambda (decl1 decl2 tenv)
    (or
     (and
      (val-decl? decl1)
      (val-decl? decl2)
      (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
     (and
      (transparent-type-decl? decl1)
      (transparent-type-decl? decl2)
      (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
     (and 
      (transparent-type-decl? decl1)
      (opaque-type-decl? decl2))
     (and
      (opaque-type-decl? decl1)
      (opaque-type-decl? decl2))
     )))

;; equiv-type? : Type * Type * Tenv -> Bool
;; Page: 311
(define equiv-type?
  (lambda (ty1 ty2 tenv)
    (equal?
     (expand-type ty1 tenv)
     (expand-type ty2 tenv))))  

  ;; type-of-program : Program -> Type
  ;; Page: 286
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (module-defs body)
          (type-of body
            (add-module-defns-to-tenv module-defs (empty-tenv)))))))

  ;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
  ;; Page: 286, 305
  (define add-module-defns-to-tenv
    (lambda (defns tenv)
      (if (null? defns)
        tenv
        (cases module-definition (car defns)
          (a-module-definition (m-name expected-iface m-body)
            (let ((actual-iface (interface-of m-body tenv)))
              (if (<:-iface actual-iface expected-iface tenv)
                ;; ok, continue in extended tenv
                (let ((new-env (extend-tenv-with-module
                                 m-name
                                 (expand-iface m-name expected-iface tenv)
                                 tenv)))
                  (add-module-defns-to-tenv (cdr defns) new-env))
                ;; no, raise error
                (report-module-doesnt-satisfy-iface m-name
                  expected-iface actual-iface))))))))

  ;; interface-of : ModuleBody * Tenv -> Iface
  ;; Page: 322
  (define interface-of
    (lambda (m-body tenv)
      (cases module-body m-body
        (defns-module-body (defns)
          (simple-iface
            (defns-to-decls defns tenv))) )))

  ;; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
  ;; Page: 288, 305
  ;; Convert defns to a set of declarations for just the names defined
  ;; in defns.  Do this in the context of tenv.  The tenv is extended
  ;; at every step, so we get the correct let* scoping
  (define defns-to-decls
    (lambda (defns tenv)
      (if (null? defns)
        '()
        (cases definition (car defns)
          (val-defn (var-name exp)
            (let ((ty (type-of exp tenv)))
              (let ((new-env (extend-tenv var-name ty tenv)))
                (cons
                  (val-decl var-name ty)
                  (defns-to-decls (cdr defns) new-env)))))
          (type-defn (name ty)
            (let ((new-env (extend-tenv-with-type
                             name
                             (expand-type ty tenv)
                             tenv)))
              (cons
                (transparent-type-decl name ty)
                (defns-to-decls (cdr defns) new-env))))))))

  (define raise-bad-module-application-error!
    (lambda (expected-type rand-type body)
      (display
        (list 'bad-module-application body
          'actual-rand-interface: rand-type
          'expected-rand-interface: expected-type))
      (eopl:error 'interface-of
        "Bad module application ~s" body)))

  (define report-module-doesnt-satisfy-iface
    (lambda (m-name expected-type actual-type)
      (display
        (list 'error-in-defn-of-module: m-name
          'expected-type: expected-type
          'actual-type: actual-type))
      (eopl:error 'type-of-module-defn)))