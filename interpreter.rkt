 ;0 Interpreter part 1
; Isaac Ng ikn3, Rahul Pokharna rkp43, Sibi Sengottuvel sxs1552

(load "simpleParser.scm")

; Function to start interpreting
(define evaluateDoc
  (lambda (fileName)
    (call/cc
     (lambda (return)
      (evaluateStatements (parser fileName) initState return initBreak)))))

; Main functiion, evaluates list of statements fed into it

;Multiple statements
(define evaluateStatements
  (lambda (stmts state return break)
    (cond
      ((null? stmts) state)
      ((list? (car stmts)) (evaluateStatements (cdr stmts) (evaluateStatement (car stmts) state return) return))
      (else (evaluateStatement stmts state return)))))
      

; Single statement
(define evaluateStatement
  (lambda (stmt state return break)
    (cond
      ((null? stmt) state)
      ; ((eq? 'try (operator expr)) (call/cc (lambda (break) (evaluateTryCatch expr state return break))))
      ((eq? 'begin (operator expr))  (removeStateLayer (call/cc (lambda (break) (evaluateStatements (blockList expr) (addStateLayer state) return break)))))
      ((eq? 'break) (break state))
      ((eq? 'if (car stmt)) (evaluateIf stmt state return))
      ((eq? 'while (car stmt)) (evaluateWhile stmt state return))
      ((eq? 'return (car stmt)) (return (evaluateExpression (returnValue stmt) state)))
      ((eq? '= (car stmt)) (evaluateAssign stmt state))
      ((eq? 'var (car stmt)) (evaluateDeclare stmt state))
      (else (evaluateExpression stmt state)))))

; add a layer to the state
(define addStateLayer
  (lambda (state)
    (cons (list '() '()) state)))

; remove a layer from the state
(define removeStateLayer
  (lambda (state)
    (cdr state)))

; Function for evaluating if statements
(define evaluateIf
  (lambda (stmt state return)
    (if (eq? 'true (evaluateExpression (ifCond stmt) state))
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
      ((null? state) #f)
      ((null? (variableList state)) #f)
      ((list? (caar state)) (or (isInState name (car state)) (isInState name (cdr state)))) ; if the state still has layers, go into the layers
      ((eq? name (car (variableList state))) #t)
      (else (isInState name (list (cdr (variableList state)) (cdr (valueList state))))))))

; Puts values into a state, removing any instance of the variable from the list first
(define putInState
  (lambda (name val state)
    (cond
      ((isInState name state) (begin (set-box! (getBoxFromState (name state)) val)) state)
      (else (cons (list (cons name (variableList (topLayer state))) (cons (box val) (valueList (topLayer state)))) (cdr state))))))


; Removes all instances of a variable from the state
(define removeFromState
  (lambda (name state)
    (cond
      ((null? state) state)
      ((null? (variableList state)) state)
      ((list? (caar state)) ( (isInState name (car state)) (isInState name (cdr state)))) ; if the state still has layers, go into the layers
      ((eq? name (car (variableList state))) (list (cdr (variableList state)) (cdr (valueList state))))
      (else (list (cons (car (variableList state)) (variableList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state))))))
                  (cons (car (valueList state)) (valueList (removeFromState name (list (cdr (variableList state)) (cdr (valueList state)))))))))))

; Gets the value of a variable from the state. If unassigned, throws an error
(define getFromState
  (lambda (name state)
    (unbox (getBoxFromState name state))))

; Gets the box from the current state
(define getBoxFromState
  (lambda (name state)
    (cond
      ((null? state) (error 'badstate "State not found"))
      ((null? (variableList state)) '())
      ((and (list? (caar state)) (isInState name (car state)) (getBoxFromState name (car state))))
      ((and (list? (caar state)) (isInState name (cdr state)) (getBoxFromState name (cdr state))))
      ((eq? name (car (variableList state))) (car (valueList state)))
      (else (getBoxFromState name (list (cdr (variableList state)) (cdr (valueList state))))))))
    
; Function to determine whether the expression will return a boolean or a number, and returns the literal values, as well as variable values.
; Lots of checks to filter which expression should go to which function
; Mvalue
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
; Mvalue
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
; Mvalue
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

; Defined for use with blocks, removes the 'begin key
(define blockList cdr)

; Defines the initial state
(define initState (list (list '() '())))

; Initial Break
;(define initBreak (error 'badbreak "Break must be in a block")) FIX THIS

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

; Used in putInState
(define topLayer car)