; Interpreter part 1
; Isaac Ng, Rahul Pokharna, Sibi Sengottuvel

(load "simpleParser.scm")

(define evaluateDoc
  (lambda (fileName)
      (evaluateStatements (parse fileName) (list '() '()) (lambda (v) v))))

(define evaluateStatements
  (lambda (stmts state return)
    (cond
      ((null? stmts) state)
      ((list? (car stmts)) (evaluateStatements (cdr stmts) (evaluateStatements (car stmts) state return) return))
      ((eq? 'if (car stmts)) (evaluateIf stmts state return))
      ((eq? 'while (car stmts)) (evaluateWhile stmts state return))
      ((eq? 'return (car stmts)) (return (evaluateExpression (returnValue stmts) state)))
      ((eq? '= (car stmts)) (evaluateAssign stmts state return))
      ((eq? 'var (car stmts)) (evaluateDeclare stmts state))
      (else (evaluateExpression lis state)))))

(define evaluateIf
  (lambda (stmt state return)
    (if (evaluateBool (ifCond stmt))
        (evaluateStatements (ifTrue stmt) state return)
        (evaluateStatements (ifFalse stmt) state return))))

(define evaluateWhile
  (lambda (stmt state return)
    (if (evaluateBool (ifCond stmt))
        (evaluateWhile stmt (evluateStatements (ifTrue stmt) state return) return))))

(define evaluateDeclare
  (lambda (stmt state)
    (putInState (variableName stmt) 'error state)))

(define evaluateAssign
  (lambda (stmt state )
    (if (isInState (variableName stmt) state)
        (putInState (variableName stmt) (variableValue stmt) state)
        (error 'depis))))

(define isInState
  (lambda (name state)
    (cond
      ((null? (variableList state)) #f)
      ((eq? name (car (variableList state))) #t)
      (else (isInState name (list (cdr (variableList state)) (cdr (valueList state))))))))

(define putInState
  (lambda (name val state)
    (list (cons name (variableList (removeFromState name state))) (cons val (valueList (removeFromState name state))))))

(define removeFromState
  (lambda (name state)
    (cond
      ((null? (variableList state)) state)
      ((eq? name (car (variableList state))) (list (cdr (variableList state)) (cdr (valueList state))))
      (else (list (cons (car (variableList state)) (variableList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state))))))
                  (cons (car (valueList state)) (valueList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state)))))))))))



; evaluateBool
; evaluateValue
; evaluateAssign
; evaluateDeclare

; Fix later

(define operator car)
    
(define operand1 cadr)

(define operand2 caddr)

(define ifCond cadr)

(define ifTrue caddr)

(define ifFalse cadddr)

(define returnValue cadr)

(define variableName cadr)

(define variableValue caddr)

(define variableList car)

(define valueList cadr)