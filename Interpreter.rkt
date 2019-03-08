#lang racket
(require "simpleParser.rkt")

;;;;Lucas Invernizzi Lmi12 EECS 345 Interpreter part 1

;;Starts the interpreting with the state empty
(define Interpret
  (lambda (filename)
    (Mstate (parser filename) '())))

;;Loops through the given parse tree to find different functions to evaluate their arguments with a given state
(define Mstate
  (lambda (e s)
    (cond
      ((and (list? (car e)) (null? (cdr e))) (Mstate (car e) s))
      ((eq? 'var (car e))                    (if (not (null? (cddr e)))
                                                 (Massign (list (leftoperand e) (Mvalue (rightoperand e) s)) s)
                                                 (Minitialize (leftoperand e) s)))
      ((eq? '= (car e))                      (if (Minitialized (leftoperand e) s)
                                                 (Massign (list (leftoperand e) (Mvalue (rightoperand e) s)) s)
                                                 (error 'uninitialized "variable not initialized")))
      ((eq? 'if (car e))                     (Mif e s))
      ((eq? 'while (car e))                  (Mwhile e s))
      ((eq? 'return (car e))                 (Mvalue (cadr e) s))
      ((list? (car e))                       (Mstate (cdr e) (Mstate (car e) s))))))

  
;;Initialize a variable (not paired with a value) to the state
(define Minitialize
  (lambda (v s)
    (append s (list (list v)))))

;;Gets a state and a variable, returns value of the variable in the state 
(define Mvariable
  (lambda (v s)
    (cond
      ((null? s)        (error 'uninitialized "variable not declared"))
      ((null? (caar s)) '())
      ((eq? v (caar s)) (if (null? (cdar s))
                            (error 'uninitilized "variable not set")
                            (cadar s)))
      (else             (Mvariable v (cdr s))))))

;;Gets a state and a variable, returns true if the state has the variable initialized
(define Minitialized
  (lambda (v s)
    (cond
      ((null? s)        #f)
      ((eq? v (caar s)) #t)
      (else             (Minitialized v (cdr s))))))

;;Precondition - Minitialized is true. Gets a variable name, value, and state. Changes variable value in the state to given value.
(define Mchange
  (lambda (e s)
    (if (eq? (car e) (caar s))
        (cons e (cdr s))
        (cons (car s) (Mchange e (cdr s))))))
    
;;Gets a list with two elements, the first the variable name, the second the value to be assigned. Checks whether the variable is already initialized
;;If it is, change the assigned value, otherwise, assign the variable.
(define Massign
  (lambda (e s)
    (cond
      ((Minitialized (car e) s) (Mchange e s))
      (else                     (append s (list e))))))

;;Performs arithmetic on a given expression to find its numeric value
(define Mvalue
  (lambda (e s)
    (cond
      ((null? e)             (error 'undefined "undefined expression"))
      ((atom? e)             (Mvariable e s))
      ((and (list? e)        (not (pair? e))) (car e))
      ((number? e)           e)
      ((eq? (operator e) '+) (+ (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s)))
      ((eq? (operator e) '-) (if (null? (cddr e))
                                 (* -1 (Mvalue (leftoperand e) s))
                                 (- (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))))
      ((eq? (operator e) '*) (* (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s)))
      ((eq? (operator e) '/) (quotient (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s)))
      ((eq? (operator e) '%) (remainder (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s)))
      (else                  (Mboolean e s)))))

;;Evaluates boolean expressions in a state
(define Mboolean
  (lambda (e s)
    (if (list? e)
        (cond
          ((eq? '< (operator e))  (if (< (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '> (operator e))  (if (> (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '<= (operator e)) (if (<= (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '>= (operator e)) (if (>= (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '== (operator e)) (if (eq? (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '!= (operator e)) (if (not (eq? (Mvalue (leftoperand e) s) (Mvalue (rightoperand e) s)))
                                 #t
                                 #f))
          ((eq? '= (operator e))  (if (and (Mboolean (leftoperand e) s) (Mboolean (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '! (operator e))  (if (not (Mboolean (leftoperand e) s))
                                 #t
                                 #f))
          ((eq? '|| (operator e)) (if (or (Mboolean (leftoperand e) s) (Mboolean (rightoperand e) s))
                                 #t
                                 #f))
          ((eq? '&& (operator e)) (if (and (Mboolean (leftoperand e) s) (Mboolean (rightoperand e) s))
                                 #t
                                 #f)))
        (cond
          ((eq? 'true e)  #t)
          ((eq? 'false e) #f)))))
  
;;Precondition - e is an if statement. Returns the state after the if statement is performed
;;cadr is the condition
;;caddr is the expression to be evaluated if the condition is true
;;cadddr is the expression to be evaluated if the condition is false
(define Mif
  (lambda (e s)
    (cond
      ((Mboolean (cadr e) s) (Mstate (caddr e) s))
      ((null? (cdddr e))     s)
      (else                  (Mstate (cadddr e) s)))))

;;Precondition - e is a while statement. Returns the state after the while loop is performed
;;cadr is the condition
;;caddr is the expression to be evaluated if the condition is true
(define Mwhile
  (lambda (e s)
    (if(Mboolean (cadr e) s)
       (Mwhile e (Mstate (caddr e) s))
       s)))

;;checks whether the given variable is a atom
(define atom?
  (lambda (e)
    (cond
      ((null? e)   #f)
      ((number? e) #f)
      ((list? e)   #f)
      ((pair? e)   #f)
      (else        #t))))

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(Interpret "test.txt")















      