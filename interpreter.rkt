; Interpreter part 1
; Isaac Ng, Rahul Pokharna, Sibi Sengottuvel

(load "simpleParser.scm")

(define evaluateDoc
  (lambda (fileName)
      (evaluateStatements (parser fileName) (list '() '()) (lambda (v) v))))

(define evaluateStatements
  (lambda (stmts state return)
    (cond
      ((null? stmts) state)
      ((list? (car stmts)) (evaluateStatements (cdr stmts) (evaluateStatements (car stmts) state return) return))
      ((eq? 'if (car stmts)) (evaluateIf stmts state return))
      ((eq? 'while (car stmts)) (evaluateWhile stmts state return))
      ((eq? 'return (car stmts)) (return (evaluateExpression (returnValue stmts) state)))
      ((eq? '= (car stmts)) (evaluateAssign stmts state))
      ((eq? 'var (car stmts)) (evaluateDeclare stmts state))
      (else (evaluateExpression stmts state)))))

;check for not having an else condition
(define evaluateIf
  (lambda (stmt state return)
    (if (eq? 'true (evaluateBool (ifCond stmt) state))
        (evaluateStatements (ifTrue stmt) state return)
        (if (not (null? (falseCheck stmt)))
            (evaluateStatements (ifFalse stmt) state return)
            state))))
        

(define evaluateWhile
  (lambda (stmt state return)
    (if (eq? 'true (evaluateBool (ifCond stmt) state))
        (evaluateWhile stmt (evaluateStatements (ifTrue stmt) state return) return)
        state)))

(define evaluateDeclare
  (lambda (stmt state)
    (cond
      ((null? (assignCheck stmt)) (putInState (variableName stmt) 'error state))
      (else (putInState (variableName stmt) (evaluateExpression (assignVal stmt) state) state)))))

(define evaluateAssign
  (lambda (stmt state)
    (if (isInState (variableName stmt) state)
        (putInState (variableName stmt) (evaluateExpression (variableValue stmt) state) state)
        (error 'Undeclared "Using a variable before declaring"))))

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

(define getFromState
  (lambda (name state)
    (cond
      ((null? (variableList state)) (error 'NotDefined))
      ((and (eq? name (car (variableList state))) (eq? 'error (car (valueList state)))) (error 'Unassigned "Variable Not Assigned"))
      ((eq? name (car (variableList state))) (car (valueList state)))
      (else (getFromState name (list (cdr (variableList state)) (cdr (valueList state))))))))

(define evaluateExpression
  (lambda (expr state)
    (cond
      ((eq? 'true expr) 'true)
      ((eq? 'false expr) 'false)
      ((number? expr) expr)
      ((isInState expr state) (getFromState expr state))
      ((not (list? expr)) (error 'Undeclared "Using a variable before declaring"))
      ((eq? '== (operator expr)) (evaluateBool expr state)) 
      ((eq? '!= (operator expr)) (evaluateBool expr state))
      ((eq? '< (operator expr)) (evaluateBool expr state))
      ((eq? '> (operator expr)) (evaluateBool expr state))
      ((eq? '<= (operator expr)) (evaluateBool expr state))
      ((eq? '>= (operator expr)) (evaluateBool expr state))
      ((eq? '&& (operator expr)) (evaluateBool expr state))
      ((eq? '|| (operator expr)) (evaluateBool expr state))
      ((eq? '! (operator expr)) (evaluateBool expr state))
      ((equal? '+ (operator expr)) (evaluateValue expr state))
      ((equal? '* (operator expr)) (evaluateValue expr state))
      ((equal? '- (operator expr)) (evaluateValue expr state))
      ((equal? '/ (operator expr)) (evaluateValue expr state))
      ((equal? '% (operator expr)) (evaluateValue expr state))
      (else (error 'NotExpression "Cannot evaluate the expression")))))

; return true and false not #t and #f
(define evaluateBool
  (lambda (expr state)
    (convertBoolToWord
     (cond
      ((eq? '== (operator expr)) (eq? (convertWordToBool (evaluateExpression (operand1 expr) state)) (convertWordToBool (evaluateExpression (operand2 expr) state)))) 
      ((eq? '!= (operator expr)) (not (eq? (convertWordToBool (evaluateExpression (operand1 expr) state)) (convertWordToBool (evaluateExpression (operand2 expr) state)))))
      ((eq? '< (operator expr)) (< (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state))) 
      ((eq? '> (operator expr)) (> (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((eq? '<= (operator expr)) (<= (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((eq? '>= (operator expr)) (>= (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((eq? '&& (operator expr)) (and (convertWordToBool (evaluateExpression (operand1 expr) state)) (convertWordToBool (evaluateExpression (operand2 expr) state))))
      ((eq? '|| (operator expr)) (or (convertWordToBool (evaluateExpression (operand1 expr) state)) (convertWordToBool (evaluateExpression (operand2 expr) state))))
      ((eq? '! (operator expr)) (not (convertWordToBool (evaluateExpression (operand1 expr) state))))
      (else (error 'NotBoolean "Cannot evaluate the boolean expression"))))))

(define convertBoolToWord
  (lambda (bool)
    (if bool
        'true
        'false)))

(define convertWordToBool
  (lambda (word)
    (cond
      ((eq? 'true word) #t)
      ((eq? 'false word) #f)
      (else word))))

; evaluateValue
(define evaluateValue
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((equal? '+ (operator expr)) (+ (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((equal? '* (operator expr)) (* (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((and (equal? '- (operator expr)) (null? (existOp2 expr))) (- 0 (evaluateExpression (operand1 expr) state)))
      ((equal? '- (operator expr)) (- (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((equal? '/ (operator expr)) (quotient (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      ((equal? '% (operator expr)) (remainder (evaluateExpression (operand1 expr) state) (evaluateExpression (operand2 expr) state)))
      (else (error 'badop "Undefined operator")))))

; Fix later

; if the first item in a list is a variable name
(define variable car)

(define val car)
        
(define operator car)
    
(define operand1 cadr)

(define operand2 caddr)

(define existOp2 cddr)

(define ifCond cadr)

(define ifTrue caddr)

(define ifFalse cadddr)

(define falseCheck cdddr)

(define returnValue cadr)

(define variableName cadr)

(define variableValue caddr)

(define variableList car)

(define valueList cadr)

(define assignCheck cddr)

(define assignVal caddr)