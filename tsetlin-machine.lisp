
(defstruct fsm
  (states '(0 1))
  (alphabet '(0 1))
  (transition-function '((0 (0 1) (1 0))
			(1 (0 0) (1 1))))
  (start-state 0)
  (current-state 0))


(defun tsetlin-transition-function-3 ()
  "each of 2n states of the given Tsetlin Finite State Automaton can take either a penalty transition 1, or reward transition 0"
  ;; Action 1
  '((0 (0 0) (1 1)) 
    (1 (0 0) (1 2))
    (2 (0 1) (1 3))
  ;; Action 2
    (3 (1 2) (0 4))
    (4 (1 3) (0 5))
    (5 (0 5) (1 4))))


(defun run-next-state (tm input)
  (let* ((tf (fsm-transition-function tm))
	 (current-state (fsm-current-state tm))
	 (next-state (cadr (assoc input
				  (cdr (assoc current-state tf)))))
	 )
    (print `(input ,input ,(case input (0 'REWARD)
			   (1 'PENALTY))
		    FROM ,current-state
		    TO STATE
		    ,next-state
		    )
	   )
    (setf (fsm-current-state tm) next-state)
    (print `(GIVING ACTION ,(tsetlin-action tm)))
    next-state))

(defun run-next-states (tm inputs)
  (loop for i in inputs do
    (run-next-state tm i)))

(defun tsetlin-transition-function (tm)
  "each of 2n states of the given Tsetlin Finite State Automaton can take either a penalty transition 1, or reward transition 0"
  (let ((num-states (length (fsm-states tm))))
    (cons
     '(0 (0 0) (1 1))
     (append
      (loop for n from 1 to (/ (1- num-states) 2)
	    collect
	    `(,n (0 ,(1- n))
		 (1 ,(1+ n))))
      (append
       (loop for n from (/ num-states 2) to (- num-states 2)
	     collect
	     `(,n (1 ,(1- n))
		  (0 ,(1+ n))))
       `((,(1- num-states)
	  (0 ,(1- num-states))
	  (1 ,(- num-states 2))))
       )))))

(defun tsetlin-action (tm)
  ;; only works for 2n 2 action transition-function  
  (let ((num-states (length (fsm-states tm))))
    (if (<= (fsm-current-state tm) (1- (/ num-states 2)))
	0 1)))
    
	 
(defun create-tsetlin-machine (n)
  (setq tm (make-fsm
	    :states (loop for x from 0 to (- (* 2 n) 1) collect x)	    
	    :start-state (random (* 2 n))))
  (setf (fsm-current-state tm) (fsm-start-state tm))
  (setf (fsm-transition-function tm) (tsetlin-transition-function tm))
  tm)

;; with input
;; inputs to tesla automata
;; calculate clause value
;; with action

;; the formula that determines clause computing
;; example 4, literal system and a single clause
;; clause is conjunctive of literals and their negations

(defun conjunction (clauses)
  (if (null clauses)
      t
      (every #'identity clauses)))

(defun negated-literals (literals)
  (mapcar #'not literals))

(defun negated-input (input)
  (mapcar (lambda (x)
	    (case x
	      (0 1)
	      (1 0)))
	  input))

(defun cij (literals)
  (conjunction
 (append (conjunction (positive-clauses))
	 (conjunction (negative-clauses))))
)

(defun input-to-boolean (input)
  (mapcar (lambda (x)
	    (case x
	      (1 t)
	      (0 nil)))))

;; the input 'votes for inclusion of literals in a clause
;; each literal can output 1 or 0 after it is included
;; 

(setq sample-input-I
      '(1 0 1 1)
      negated-input-Ibar
      (negated-input sample-input-I)
      resulting-clij-calculated-positive
      "/\ x0 x2 x3" ;; include/exclude literals?
      resulting-clij-calculated-negative
      "/\ x1"
      resulting-c
      "1 & 1 & 1 & 1....&1 = 1"
      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positive-clauses (i)
  ())

(defun negative-clauses (i)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;

	

(let ((machine (create-tsetlin-machine 3)))
  (print (fsm-current-state machine))
  (print (tsetlin-action machine)))
