; Interpreter part 1
; Isaac Ng, Rahul Pokharna, Sibi Sengottuvel

(load "simpleParser.scm")

(define evaluateDoc
  (lambda
      (evaluateStatements (parse "test.txt") (list '() '()) (lambda (v) v))

(define evaluateStatements
  (lambda (stmts state return)
    (cond
      ((null? stmts) state)
      ((list? (car stmts)) (evaluateStatements (cdr stmts) (evaluateStatements (car stmts) state return) return))
      ((eq? 'if (car stmts)) (evaluateIf stmts state return))
      ((eq? 'while (car stmts)) (evaluateIf stmts state return))
      ((eq? 'return (car stmts)) (return (evaluateExpression (returnValue stmts) state)))
      ((eq? '= (car stmts)) (evaluateIf stmts state return))
      ((eq? 'var (car stmts)) (evaluateIf stmts state return))
      (else (evaluateExpression lis state)))))

(define evaluateIf
  (lambda (stmt state return)
    (if (evaluateBool (ifCond stmt))
        (evaluateStatements (ifTrue stmt) state return)
        (evaluateStatements (ifFalse stmt) state return))))
    

; evaluateBool
; evaluateValue
; evaluateWhile
; evaluateAssign
; 

; Fix later

(define operator car)
    
(define operand1 cadr)

(define operand2 caddr)

(define ifCond cadr)

(define ifTrue caddr)

(define ifFalse cadddr)

(define returnValue cadr)