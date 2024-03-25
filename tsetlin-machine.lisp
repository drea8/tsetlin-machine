
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

	

(let ((machine (create-tsetlin-machine 3)))
  (print (fsm-current-state machine))
  (print (tsetlin-action machine)))
