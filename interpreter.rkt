; Interpreter part 1
; Isaac Ng ikn3, Rahul Pokharna rkp43, Sibi Sengottuvel sxs1552

(load "simpleParser.scm")

; Function to start interpreting
(define evaluateDoc
  (lambda (fileName)
      (evaluateStatements (parser fileName) (list '() '()) (lambda (v) v))))

; Main functiion, evaluates list of statements fed into it
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

; Function for evaluating if statements
(define evaluateIf
  (lambda (stmt state return)
    (if (eq? 'true (evaluateBool (ifCond stmt) state))
        (evaluateStatements (ifTrue stmt) state return)
        (if (not (null? (falseCheck stmt)))
            (evaluateStatements (ifFalse stmt) state return)
            state))))
        
; Function to evaluate while loops
(define evaluateWhile
  (lambda (stmt state return)
    (if (eq? 'true (evaluateBool (ifCond stmt) state))
        (evaluateWhile stmt (evaluateStatements (ifTrue stmt) state return) return)
        state)))

; Function to evaluate declaration, adding it to the state
(define evaluateDeclare
  (lambda (stmt state)
    (cond
      ((null? (assignCheck stmt)) (putInState (variableName stmt) 'error state))
      (else (putInState (variableName stmt) (evaluateExpression (assignVal stmt) state) state)))))

; Function to add a value to the state with an associated variable. Throws an error if the variable has not been declared yet
(define evaluateAssign
  (lambda (stmt state)
    (if (isInState (variableName stmt) state)
        (putInState (variableName stmt) (evaluateExpression (variableValue stmt) state) state)
        (error 'Undeclared "Using a variable before declaring"))))

; Determines whether a variable is in the state, i.e. if it has been declared and/or assigned
(define isInState
  (lambda (name state)
    (cond
      ((null? (variableList state)) #f)
      ((eq? name (car (variableList state))) #t)
      (else (isInState name (list (cdr (variableList state)) (cdr (valueList state))))))))

; Puts values into a state, removing any instance of the variable from the list first
(define putInState
  (lambda (name val state)
    (list (cons name (variableList (removeFromState name state))) (cons val (valueList (removeFromState name state))))))

; Removes all instances of a variable from the state
(define removeFromState
  (lambda (name state)
    (cond
      ((null? (variableList state)) state)
      ((eq? name (car (variableList state))) (list (cdr (variableList state)) (cdr (valueList state))))
      (else (list (cons (car (variableList state)) (variableList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state))))))
                  (cons (car (valueList state)) (valueList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state)))))))))))

; Gets the value of a variable from the state. If unassigned, throws an error
(define getFromState
  (lambda (name state)
    (cond
      ((null? (variableList state)) (error 'NotDefined))
      ((and (eq? name (car (variableList state))) (eq? 'error (car (valueList state)))) (error 'Unassigned "Variable Not Assigned"))
      ((eq? name (car (variableList state))) (car (valueList state)))
      (else (getFromState name (list (cdr (variableList state)) (cdr (valueList state))))))))

; Function to determine whether the expression will return a boolean or a number, and returns the literal values, as well as variable values.
; Lots of checks to filter which expression should go to which function
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

; Function to return true or false given an expression
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

; Helper function to assist in conversion between words and boolean values
(define convertBoolToWord
  (lambda (bool)
    (if bool
        'true
        'false)))

; Helper function to assist in conversion between boolean values and words
(define convertWordToBool
  (lambda (word)
    (cond
      ((eq? 'true word) #t)
      ((eq? 'false word) #f)
      (else word))))

; Function to return the exact value of a function given an expression, returning a numeral value
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

; Abstraction below

; if the first item in a list is a variable name
(define variable car)

; Returns the value from the valueList
(define val car)

; returns the operator from an expression
(define operator car)

; returns the first operand from an expression
(define operand1 cadr)

; returns the second operand from an expression
(define operand2 caddr)

; Determines whether the second operator exists or not
(define existOp2 cddr)

; Gets the if condition of while and if statements
(define ifCond cadr)

; Gets the condition to execute if the condition is true
(define ifTrue caddr)

; Gets the condition to execute if the condition is false
(define ifFalse cadddr)

; Determines whether there is an else statement for an if statement
(define falseCheck cdddr)

; Gets the value to be returned from a return statement
(define returnValue cadr)

; gets the name for a variable, used in assignment and declaration
(define variableName cadr)

; gets the value for a variable, used in assignment
(define variableValue caddr)

; Returns the list of variables from the state
(define variableList car)

; Returns the list of values from the state
(define valueList cadr)

; checks whether there is an assignment after a declaration
(define assignCheck cddr)

; Used in assignment after a delcaration
(define assignVal caddr)