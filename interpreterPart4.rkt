;0 Interpreter part 1
; Isaac Ng ikn3, Rahul Pokharna rkp43, Sibi Sengottuvel sxs1552

(load "classParser.scm")

; Function to start interpreting
;;; (define evaluateDoc
;;;   (lambda (fileName)
;;;     (call/cc
;;;      (lambda (return)
;;;        (evaluateFunction 'main '() (evaluateStatements (parser fileName) initState return initBreak initCont initThrow) return initBreak initCont initThrow )))))

(define interpret
  (lambda (fileName className)
    (call/cc
     (lambda (return)
       (runMain className (evaluateStatements (parser fileName) (initClassClosures (parser fileName) initState return initBreak initCont initThrow className) return initBreak initCont initThrow className) return initBreak initCont initThrow className)))))

; Main functiion, evaluates list of statements fed into it
(define runMain
  (lambda (className state return break continue throw type)
    (evaluateStaticFunction 'main '() (putInState 'main (getFromState 'main (operand3 (getFromState className state))) 
      (addStateLayer state)) return break continue throw type)))

(define initClassClosures ;just get the boxes for each class in the state wihtout making the closures yet.
  (lambda (stmt state return break continue throw type)
    (cond
      ((null? stmt) state)
      ((list? (car stmt)) (initClassClosures (cdr stmt) (initClassClosures (car stmt) state return break continue throw type) return break continue throw type))
      ((eq? 'class (operator stmt))  (initClassClosures (cdr stmt) (putInState (operand1 stmt) '() state) return break continue throw type))
      (else state))))

;Multiple statements
(define evaluateStatements
  (lambda (stmts state return break continue throw type)
    (cond
      ((null? stmts) state)
      ((list? (car stmts)) (evaluateStatements (cdr stmts) (evaluateStatement (car stmts) state return break continue throw type) return break continue throw type))
      (else (evaluateStatement stmts state return break continue throw type)))))
      

; Single statement
(define evaluateStatement
  (lambda (stmt state return break continue throw type)
    (cond
      ((null? stmt) state)
      ((eq? 'class (operator stmt))  (evaluateClassClosure stmt state return break continue throw type))
      ((eq? 'begin (operator stmt))  (evaluateBlock stmt state return break continue throw type))
      ((eq? 'break (operator stmt)) (break state))
      ((eq? 'if (car stmt)) (evaluateIf stmt state return break continue throw type))
      ((eq? 'while (car stmt)) (call/cc
                                (lambda (breaknew)
                                  (evaluateWhile stmt state return (lambda (s) (breaknew (removeStateLayer s))) continue throw type))))
      ((eq? 'return (car stmt)) (return (evaluateExpression (returnValue stmt) state return break continue throw type)))
      ((eq? '= (car stmt)) (evaluateAssign stmt state return break continue throw type))
      ((eq? 'var (car stmt)) (evaluateDeclare stmt state return break continue throw type))
      ((eq? 'try (car stmt)) (evaluateTryCatchBlock stmt state return break continue throw type))
      ;TODO ((eq? 'continue (car stmt)) (con
      ((eq? 'throw (car stmt)) (throw state (operand1 stmt)))
      ((eq? 'continue (car stmt)) (continue state))
      ((eq? 'function (car stmt)) (evaluateFunctionClosure stmt state return break continue throw type))
      ((eq? 'funcall (operator stmt)) (evaluateFunction (cadr stmt) (cddr stmt) state return break continue throw type (getLeftSideOfDot (cadadr stmt) state return break continue throw type)) state);what is this state
      ((eq? 'dot (operator stmt)) (evaluateDot (getLeftSideOfDot (operand1 stmt) state return break continue throw type) (operand2 stmt) state return break continue throw type)
      (else (evaluateExpression stmt state return break continue throw type))))))

; Function to determine whether the expression will return a boolean or a number, and returns the literal values, as well as variable values.
; Lots of checks to filter which expression should go to which function
; Mvalue
(define evaluateExpression
  (lambda (expr state return break continue throw type)
    (cond
      ((eq? 'true expr) 'true)
      ((eq? 'false expr) 'false)
      ((number? expr) expr)
      ((isInState expr state) (getFromState expr state))
      ((not (list? expr))  (error 'Undeclared "Using a variable before declaring"))
      ((eq? 'funcall (operator expr)) (evaluateFunction (cadr expr) (cddr expr) state return break continue throw type (getLeftSideOfDot (cadadr expr) state return break continue throw type)))
      ((eq? 'dot (operator expr)) (evaluateDot (getLeftSideOfDot (operand1 expr) state return break continue throw type) (operand2 expr) state return break continue throw type))
      ((eq? 'new (operator expr))  (evaluateInstanceClosure expr state return break continue throw type))
      ((eq? '== (operator expr)) (evaluateBool expr state return break continue throw type)) 
      ((eq? '!= (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '< (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '> (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '<= (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '>= (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '&& (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '|| (operator expr)) (evaluateBool expr state return break continue throw type))
      ((eq? '! (operator expr)) (evaluateBool expr state return break continue throw type))
      ((equal? '+ (operator expr)) (evaluateValue expr state return break continue throw type))
      ((equal? '* (operator expr)) (evaluateValue expr state return break continue throw type))
      ((equal? '- (operator expr)) (evaluateValue expr state return break continue throw type))
      ((equal? '/ (operator expr)) (evaluateValue expr state return break continue throw type))
      ((equal? '% (operator expr)) (evaluateValue expr state return break continue throw type))
      (else (error 'NotExpression "Cannot evaluate the expression")))))


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
  (lambda (stmt state return break continue throw type)
    (if (eq? 'true (evaluateExpression (ifCond stmt) state return break continue throw type))
        (evaluateStatement (ifTrue stmt) state return break continue throw type)
        (if (not (null? (falseCheck stmt)))
            (evaluateStatement (ifFalse stmt) state return break continue throw type)
            state))))
        
; Function to evaluate while loops
(define evaluateWhile
  (lambda (stmt state return break continue throw type)
    (if (eq? 'true (evaluateBool (ifCond stmt) state return break continue throw type))        
        (evaluateWhile stmt (call/cc
                             (lambda (continuenew)
                               (evaluateStatement (ifTrue stmt) state return break (lambda (s) (continuenew (removeStateLayer s))) throw type))) return break continue throw type) ; removed remoivestatelayer 
        state)))

; Function to evaluate declaration, adding it to the state
(define evaluateDeclare
  (lambda (stmt state return break continue throw type)
    (cond
      ((null? (assignCheck stmt)) (putInStateDeclare (variableName stmt) 'error state))
      (else (putInStateDeclare (variableName stmt) (evaluateExpression (assignVal stmt) state return break continue throw type) state)))))

; Function to add a value to the state with an associated variable. Throws an error if the variable has not been declared yet
(define evaluateAssign
  (lambda (stmt state return break continue throw type)
    (if (list? (variableName stmt))
        (if (hasField (operand2 (variableName stmt)) (getLeftSideOfDot (operand1 (variableName stmt)) state return break continue throw type));its a dot
            (setField
             (operand2 (variableName stmt))
             (evaluateExpression (variableValue stmt) state return break continue throw type)
             (operand1 (variableName stmt))
             (getLeftSideOfDot (operand1 (variableName stmt)) state return break continue throw type)
             state)
            (error 'Undeclared "Using a variable before declaring"))
        (if (isInState (variableName stmt) state)
            (putInState (variableName stmt) (evaluateExpression (variableValue stmt) state return break continue throw type) state)
            (error 'Undeclared "Using a variable before declaring")))))

(define getField
  (lambda (fieldName instanceClosure)
    (getFromState fieldName (operand1 instanceClosure))))

(define hasField
  (lambda (fieldName instanceClosure)
    (isInState fieldName (operand1 instanceClosure))))

(define setField ;returns the updated state with the updated closure
  (lambda (fieldName value instanceName instanceClosure state)
    (putInState instanceName (putInState fieldName value (operand1 instanceClosure)) state)))


; Function to evaluate the function closure [parameter names, body, state, return, break, continue, throw, type, thisClosure] 
(define evaluateFunctionClosure
  (lambda (stmt state return break continue throw type)
    (putInState (funcName stmt) (list (funcParam stmt) (funcBody stmt) state return break continue throw type) state)))

;new Function to evaluate the function closure
(define evaluateFunctionClosure2
  (lambda (stmt flist state return break continue throw type)
    (putInState (funcName stmt) (list (cons 'this (funcParam stmt)) (funcBody stmt) state return break continue throw type) flist))) ;cons this onto the funcparams

;new Function to evaluate the function closure
(define evaluateStaticFunctionClosure
  (lambda (stmt flist state return break continue throw type)
    (putInState (funcName stmt) (list (funcParam stmt) (funcBody stmt) state return break continue throw type) flist))) ;cons this onto the funcparams

; Function to evaluate the class closure
; Class closure format: List[parentClassName, list of fields, list of functions, list of main function]
(define evaluateClassClosure
  (lambda (stmt state return break continue throw type)
    (putInState (operand1 stmt) (list (getParentName (operand2 stmt)) 
      (evaluateInstanceVariables (operand3 stmt) initState return break continue throw type)
      (evaluateInstanceFunctions (operand3 stmt) initState state return break continue throw type)
      (evaluateMainFunction (operand3 stmt) initState state return break continue throw type)) state)))

;This returns a closure, not a state Closure: [class closure, list of fields, parent closure]
(define evaluateInstanceClosure
  (lambda (stmt state return break continue throw type)
    (list (getFromState (operand1 stmt) state) (copylist (operand1 (getFromState (operand1 stmt) state)) initState) (parentClosure (car (getFromState (operand1 stmt) state)) state))));get list of fields from the closure for this class.

(define parentClosure
  (lambda (parentClassName state)
    (if (isInState parentClassName state)
      (list (getFromState parentClassName state) (copylist (operand1 (getFromState parentClassName state)) initState) (parentClosure (car (getFromState parentClassName state)) state))
      '())))

; Function to get the parent name from operand2 of the class stmt
(define getParentName
  (lambda (l) 
    (if (null? l)
      'Object
      (operand1 l))))

; Function to get the list of instance variables given the body.
(define evaluateInstanceVariables
  (lambda (stmt state return break continue throw type)
    (cond
      ((null? stmt) state)
      ((list? (car stmt)) (evaluateInstanceVariables (cdr stmt) (evaluateInstanceVariables (car stmt) state return break continue throw type) return break continue throw type))
      ((eq? 'var (car stmt)) (evaluateDeclare stmt state return break continue throw type))
      (else state))))

; Function to get the list of instance fucntions given the body.
(define evaluateInstanceFunctions ;TODO when we call a function, we should put the state from the class closure into the function closure, and not the one we pass in here, because it may not be fully updated with all the other functions.
  (lambda (stmt flist state return break continue throw type)
    (cond
      ((null? stmt) flist)
      ((list? (car stmt)) (evaluateInstanceFunctions (cdr stmt) (evaluateInstanceFunctions (car stmt) flist state return break continue throw type) state return break continue throw type))
      ((eq? 'function (car stmt)) (evaluateFunctionClosure2 stmt flist state return break continue throw type))
      (else flist))))

; returns "state" with closure for main function if it exists.
(define evaluateMainFunction 
  (lambda (stmt flist state return break continue throw type)
    (cond
      ((null? stmt) flist)
      ((list? (car stmt)) (evaluateMainFunction (cdr stmt) (evaluateMainFunction (car stmt) flist state return break continue throw type) state return break continue throw type))
      ((eq? 'static-function (car stmt)) (evaluateStaticFunctionClosure stmt flist state return break continue throw type))
      (else flist))))

; Function to execute a function, given the input parameters
(define evaluateFunction
  (lambda (name params state return break continue throw type instanceClosure)
    (call/cc
     (lambda (newReturn)
       (if (eq? (length (car (evaluateExpression name state return break continue throw type))) (+ 1 (length params)));add 1 for this.
           (evaluateFunctionBlock (cadr (evaluateExpression name state return break continue throw type)) (addParams1 (car (evaluateExpression name state return break continue throw type)) (cons instanceClosure params)
                                                                                                                      (addStateLayer (caddr (evaluateExpression name state return break continue throw type))) return break continue throw type state) newReturn break continue throw type)
           (throw state "wrong number of arguments"))))))

(define evaluateStaticFunction
  (lambda (name params state return break continue throw type)
    (call/cc
     (lambda (newReturn)
       (if (eq? (length (car (getFromState name state))) (length params))
           (evaluateFunctionBlock (cadr (getFromState name state)) (addParams (car (getFromState name state)) params (addStateLayer (putInState name (getFromState name state) (caddr (getFromState name state)))) return break continue throw type state) newReturn break continue throw type)
           (throw state "wrong number of arguments"))))))

; Associates the parameters in the closure to the input parameters
(define addParams
  (lambda (paramNames paramValues state return break continue throw type oldState)
    (if (null? paramNames)
        state
        (addParams (cdr paramNames) (cdr paramValues) (putInState (car paramNames) (evaluateExpression (car paramValues) oldState return break continue throw type) state) return break continue throw type oldState))))

(define addParams1
  (lambda (paramNames paramValues state return break continue throw type oldState)
    (if (null? paramNames)
        state
        (addParams (cdr paramNames) (cdr paramValues) (putInState (car paramNames) (car paramValues) state) return break continue throw type oldState))))
                           
    
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
      ((isInState name state) (begin (set-box! (getBoxFromState name state) val)) state)
      (else (cons (list (cons name (variableList (topLayer state))) (cons (box val) (valueList (topLayer state)))) (cdr state))))))

; Used for putting variables in scope, static scoping
(define putInStateDeclare
  (lambda (name val state)
    (cond
      ((isInState name (car state)) (begin (set-box! (getBoxFromState name state) val)) state)
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
    

; Function to return true or false given an expression
; Mvalue
(define evaluateBool
  (lambda (expr state return break continue throw type)
    (convertBoolToWord
     (cond
       ((eq? 'true expr) #t)
       ((eq? 'false expr) #f)
       ((eq? '== (operator expr)) (eq? (convertWordToBool (evaluateExpression (operand1 expr) state return break continue throw type)) (convertWordToBool (evaluateExpression (operand2 expr) state return break continue throw type)))) 
       ((eq? '!= (operator expr)) (not (eq? (convertWordToBool (evaluateExpression (operand1 expr) state return break continue throw type)) (convertWordToBool (evaluateExpression (operand2 expr) state return break continue throw type)))))
       ((eq? '< (operator expr)) (< (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type))) 
       ((eq? '> (operator expr)) (> (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
       ((eq? '<= (operator expr)) (<= (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
       ((eq? '>= (operator expr)) (>= (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
       ((eq? '&& (operator expr)) (and (convertWordToBool (evaluateExpression (operand1 expr) state return break continue throw type)) (convertWordToBool (evaluateExpression (operand2 expr) state return break continue throw type))))
       ((eq? '|| (operator expr)) (or (convertWordToBool (evaluateExpression (operand1 expr) state return break continue throw type)) (convertWordToBool (evaluateExpression (operand2 expr) state return break continue throw type))))
       ((eq? '! (operator expr)) (not (convertWordToBool (evaluateExpression (operand1 expr) state return break continue throw type))))
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

; used to make new boxes for each instance variable in instance closure
(define copylist
  (lambda (oldList state)
    (cond
      ((null? oldList) state)
      ((null? (car oldList)) state)
      ((pair? (caar oldList)) (copylist (cdr oldList) (copylist (car oldList) (addStateLayer state))))
      (else (copylist (list (cdar oldList) (cdadr oldList)) (putInState (caar oldList) (getFromState (caar oldList) oldList) state))))))

; Function to return the exact value of a function given an expression, returning a numeral value
; Mvalue
(define evaluateValue
  (lambda (expr state return break continue throw type)
    (cond
      ((number? expr) expr)
      ((equal? '+ (operator expr)) (+ (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
      ((equal? '* (operator expr)) (* (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
      ((and (equal? '- (operator expr)) (null? (existOp2 expr))) (- 0 (evaluateExpression (operand1 expr) state return break continue throw type)))
      ((equal? '- (operator expr)) (- (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
      ((equal? '/ (operator expr)) (quotient (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
      ((equal? '% (operator expr)) (remainder (evaluateExpression (operand1 expr) state return break continue throw type) (evaluateExpression (operand2 expr) state return break continue throw type)))
      (else (error 'badop "Undefined operator")))))

; Evaluates the try catch block overall
(define evaluateTryCatchBlock
  (lambda (stmt state return break continue throw type)
    (evaluateFinally (finallyBody stmt) (call/cc
                                         (lambda (finish)
                                           (finish (evaluateTryBlock (tryBody stmt) state return  break continue (lambda (error_state my_error)
                                                                                                                   (finish (evaluateCatch (catchBody stmt) state (evaluateExpression my_error error_state return break continue throw type) return break continue throw type)))))))
                     return break continue throw type)))
;evaluate catch block
(define evaluateCatch
  (lambda (catchList state error return break continue throw type)
    (if (null? catchList)
        state
        (evaluateCatchBlock catchList state error return break continue throw type))))

;evaluate finally if it exists
(define evaluateFinally
  (lambda (finallyList state return break continue throw type)
    (if (null? finallyList)
        state
        (evaluateTryBlock (operand1 finallyList) state return break continue throw type))))
    
;evaluateBlock
(define evaluateBlock
  (lambda (stmts state return break continue throw type)
    (removeStateLayer (evaluateStatements (cdr stmts) (addStateLayer state) return break continue throw type))))

; Evaluates the try block individually
(define evaluateTryBlock
  (lambda (stmts state return break continue throw type)
    (removeStateLayer (evaluateStatements stmts (addStateLayer state) return break continue throw type))))

; Evaluate the catch block individually
(define evaluateCatchBlock
  (lambda (stmts state error return break continue throw type)
    (removeStateLayer (evaluateStatements (operand2 stmts) (putInState (exception stmts) error (addStateLayer state)) return break continue throw type))))

; Evaluate the function block individually
(define evaluateFunctionBlock
  (lambda (stmts state return break continue throw type)
    (removeStateLayer (evaluateStatements stmts (addStateLayer state) return break continue throw type))))

(define evaluateDot
  (lambda (instanceClosure name state return break continue throw type)
    (if (hasFunction name instanceClosure) ;check if the function name is in the functionlist in the class closure, which we get using the classname from the instance closure.
       (getFunction name instanceClosure) ;we know this is a function call. so the goal is to return the function closure. Also append the instance onto the end of the function closure.
       (getFromState name (cadr instanceClosure))
    )))

(define hasFunction
  (lambda (name instanceClosure)
    (if (isInState name (functionListOP (classNameOP instanceClosure)))
        #t
        (if (null? (operand2 instanceClosure))
            #f
            (hasFunction name (operand2 instanceClosure))))))

(define getFunction
  (lambda (name instanceClosure)
    (if (isInState name (functionListOP (classNameOP instanceClosure)))
        (getFromState name (functionListOP (classNameOP instanceClosure)))
        (if (null? (operand2 instanceClosure))
            '()
            (getFunction name (operand2 instanceClosure))))))

(define getLeftSideOfDot
  (lambda (instanceName state return break continue throw type)
    (evaluateExpression instanceName state return break continue throw type)))


; Abstraction below

; Defined for use with blocks, removes the 'begin key
(define blockList cdr)

; Defines the initial state
(define initState (list (list '() '())))

; Define empty level of a state
(define emptyState (list '() '()))

; Initial Break
(define initBreak (lambda (s) (error 'badbreak "Break must be in a block")))

; Initial Continuation
(define initCont (lambda (s) (error 'badcont "Continue must be in a loop")))

; Initial Throw
(define initThrow (lambda (s e) (error 'UncaughtException e)))

;For use with class closure List[parentClassName, list of fields, list of functions, list of main function]
(define functionListOP caddr)

;For use with instance closure
(define classNameOP car)



(define index
  (lambda (l i)
    (if (eq? i 0)
      (car l)
      (index (cdr l) (- i 1)))))

; For use with function closure creation
(define funcName cadr)
(define funcParam caddr)
(define funcBody cadddr)



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

;returns the thrid operand
(define operand3 cadddr)
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

;Used for catch
(define exception caadr)

;try-catch abstractions:
(define tryBody cadr)
(define catchBody caddr)
(define finallyBody cadddr)