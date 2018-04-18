(define (make-arrayStack)

  ;attributes
  (define arr '())
  (define length 0)


  (define (print) arr)
  (define (size) length)


  ;ADDING FUNCTIONALITY BEGINS-------------------------------------------------------------
  
  ;i = index to add element at
  ;x = element to add

  ;this part of the adding functionality is to error check
  ;it will call the addOK function if there is nothing wrong with the inputs
  (define (add i x)
    (cond ((> i (+ length 1)) (display "Can't add there; requested index too large")(newline))
          ((> 0 i)            (display "Can't add there; requested index too small")(newline))
          (else (addOK i x))))
    
  (define (addOK i x)
    (set! length (+ length 1))
    (cond  ((= i 0) (addHead x))
           (else (set! arr (addHelper i x arr 0)))))

  
  ;recursive function to add element x to indices other than 0
  (define (addHelper i x structure currIndex)
    (cond ((= i currIndex) (cons x structure))
          (else (cons (car structure) (addHelper i x (cdr structure) (+ currIndex 1)))))
  )

  ;adds element x to index 0 only
  (define (addHead x)
    (set! arr (cons x arr))
  )
  
  ;ADDING FUNCTIONALITY FINISHES-----------------------------------------------------------


  ;SETTING FUNCTIONALITY BEGINS------------------------------------------------------------

  ;i = index to add element at
  ;x = element to add

  (define (set i x)
    (cond ((>= i length) (display "Can't set there; requested index too large")(newline))
          ((> 0 i)       (display "Can't set there; requested index too small")(newline))
          (else (setOK i x))))
  
  (define (setOK i x)
    (cond ((= i 0) (setHead x))
          (else (set! arr (setHelper i x arr 0)))))

  (define (setHelper i x structure currIndex)
    (cond ((= i currIndex) (cons x (cdr structure)))
          (else (cons (car structure) (setHelper i x (cdr structure) (+ currIndex 1))))))

  
  (define (setHead x)
    (set! arr (cons x (cdr arr))))
  
  ;SETTING FUNCTIONALITY FINISHES----------------------------------------------------------
  

  ;GETTING FUNCTIONALITY BEGINS------------------------------------------------------------

  ;i = index to get

  (define (get i)
    (cond ((>= i length) (display "Can't get there; requested index too large")(newline))
          ((> 0 i)        (display "Can't get there; requested index too small")(newline))
          (else (getOK i))))

  (define (getOK i)
    (cond ((= i 0) (getHead))
          (else (getHelper i arr 0))))

  (define (getHelper i structure currIndex)
    (cond ((= i currIndex) (car structure))
          (else (getHelper i (cdr structure) (+ currIndex 1)))))

  (define (getHead)
    (car arr))


  ;GETTING FUNCTIONALITY FINISHES----------------------------------------------------------


  ;REMOVING FUNCTIONALITY BEGINS-----------------------------------------------------------

  (define (remove i)
    (cond ((>= i length) (display "Can't remove there; requested index too large")(newline))
          ((> 0 i)       (display "Can't remove there; requested index too small")(newline))
          (else (removeOK i))))


  (define (removeOK i)
    (set! length (- length 1))
    (cond ((= i 0) (removeHead))
          (else (set! arr (removeHelper i arr 0)))))

  (define (removeHelper i structure currIndex)
    (cond ((= i currIndex) (cdr structure)) 
          (else (cons (car structure) (removeHelper i (cdr structure) (+ currIndex 1))))))

  
  (define (removeHead)
    (set! arr (cdr arr)))
 


  ;REMOVING FUNCTIONALITY FINISHES---------------------------------------------------------

  ;method to get the method from the user,
  ;how to interact from interactions pane or inline code shown below
  (define (dispatch method)
    (cond ((eq? method 'print) print)
          ((eq? method 'size) size)
          ((eq? method 'add) add)
          ((eq? method 'get) get)
          ((eq? method 'set) set)
          ((eq? method 'remove) remove)
          (else (lambda() (display "Unknown Request: ")(display method)(newline)))))

  dispatch)


;TESTING --------------------------------------------------------------------------------------------------

(define (test-remove)
  (display "TESTING REMOVE")(newline)
  (define arrayStack (make-arrayStack))
  (display ((arrayStack 'print)))(newline)

  ;setting up the arrayStack
  ((arrayStack 'add) 0 5)
  ((arrayStack 'add) 0 4)
  ((arrayStack 'add) 0 3)
  ((arrayStack 'add) 0 2)
  ((arrayStack 'add) 0 1)
  ((arrayStack 'add) 0 0)
  (display ((arrayStack 'print)))(newline)

  ;testing remove on the front index
  (display "testing remove on the front index")(newline)
  ((arrayStack 'remove) 0)
  (display ((arrayStack 'print)))(newline)

  ;testing remove on the end index
  (display "testing remove on the end index")(newline)
  ((arrayStack 'remove) 4)
  (display ((arrayStack 'print)))(newline)

  ;testing remove on a sandwhiched index
  (display "testing remove on a sandwhiched index")(newline)
  ((arrayStack 'remove) 2)
  (display ((arrayStack 'print)))(newline)

  ;testing remove on an index larger then length
  (display "testing remove on index larger then the length")(newline)
  ((arrayStack 'remove) 3)
  (display ((arrayStack 'print)))(newline)

  ;testing remove on an index smaller then 0
  (display "testing remove on index smaller then 0")(newline)
  ((arrayStack 'remove) -1)
  (display ((arrayStack 'print)))(newline)

)

;(test-remove)(newline)

(define (test-set)
  (display "TESTING SET")(newline)
  (define arrayStack (make-arrayStack))
  (display ((arrayStack 'print)))(newline)

  ;setting up the arrayStack
  ((arrayStack 'add) 0 2)
  ((arrayStack 'add) 0 1)
  ((arrayStack 'add) 0 0)
  (display ((arrayStack 'print)))(newline)

  ;testing set on the front index
  (display "testing set on the front index")(newline)
  ((arrayStack 'set) 0 9)
  (display ((arrayStack 'print)))(newline)

  ;testing set on the end index
  (display "testing set on the end index")(newline)
  ((arrayStack 'set) 2 9)
  (display ((arrayStack 'print)))(newline)

  ;testing set on a sandwhiched index
  (display "testing set on a sandwhiched index")(newline)
  ((arrayStack 'set) 1 9)
  (display ((arrayStack 'print)))(newline)

  ;testing set on a index larger then length
  (display "testing set on a index larger then length")(newline)
  ((arrayStack 'set) 3 9)

  ;testing set on index smaller then 0
  (display "testing set on index smaller then 0")(newline)
  ((arrayStack 'set) -1 9)
  
)

;(test-set)(newline)

(define (test-get)

  (display "TESTING GET")(newline)
  (define arrayStack (make-arrayStack))
  (display ((arrayStack 'print)))(newline)

  ;setting up the arrayStack
  ((arrayStack 'add) 0 2)
  ((arrayStack 'add) 0 1)
  ((arrayStack 'add) 0 0)
  (display ((arrayStack 'print)))(newline)
  
  ;testing get in the 'front' index (i.e in the head position index 0)
  (display "Testing get on front index")(newline)
  (display ((arrayStack 'get) 0))(newline)

  ;testing get in the 'end' index (i.e index = length)
  (display "Testing get on end index")(newline)
  (display ((arrayStack 'get) 2))(newline)

  ;testing get in a sandwhiched index (i.e the desired index is between two other elements)
  (display "Testing get on a sandwhiced index")(newline)
  (display ((arrayStack 'get) 1))(newline)

  ;testing get on index greater then and equal length
  (display "Testing get on index greater then and equal to length")(newline)
  (display ((arrayStack 'get) 3))(newline)
  (display ((arrayStack 'get) 2))(newline)

  ;testing get on index smaller then 0
  (display "Testing get on index smaller then 0")(newline)
  (display ((arrayStack 'get) -1))(newline)
)

;(test-get)(newline)
  

(define (test-add)
  ;testing, creation and syntax to interact with this 'object'
  ;make the arrayStack

  (display "TESTING ADD")(newline)
  (define arrayStack (make-arrayStack))
  ;call a function on the arrayStack (in this example it is print)
  (display ((arrayStack 'print)))(newline)

  ;testing add in the 'front' index (i.e in the head position, index 0)
  (display "Testing add in front index")(newline)
  ((arrayStack 'add) 0 2)
  ((arrayStack 'add) 0 1)
  ((arrayStack 'add) 0 0)
  (display ((arrayStack 'print)))(newline)

  ;testing add in a 'middle' index (i.e one that is sandwhiched between two other values)
  (display "Testing add in sandwhiched index")(newline)
  ((arrayStack 'add) 1 9)
  (display ((arrayStack 'print)))(newline)
  ;((arrayStack 'size))

  ;testing add in an 'end' index (i.e one that the desired insert index is equal to length+1)
  (display "Testing add when length+1 = index")(newline)
  ((arrayStack 'add) 4 8)
  (display ((arrayStack 'print)))(newline)
  ;((arrayStack 'size))

  ;testing add in an 'near end' index (i.e one that the desired insert index is equal to length)
  (display "Testing add when length = index")(newline)
  ((arrayStack 'add) 4 7)
  (display ((arrayStack 'print)))(newline)
  ;((arrayStack 'size))

  ;testing add in when index > length+1
  (display "Testing add when index > length+1")(newline)
  ((arrayStack 'add) 8 6)
  (display ((arrayStack 'print)))(newline)
  ;((arrayStack 'size))

  ;testing add in a negative index (i.e index below zero)
  (display "Testing add when index < 0")(newline)
  ((arrayStack 'add) -1 7)
  (display ((arrayStack 'print)))(newline)
  ;((arrayStack 'size))
)

;(test-add)(newline)


(test-add)(newline)
(test-get)(newline)
(test-set)(newline)
(test-remove)(newline)


