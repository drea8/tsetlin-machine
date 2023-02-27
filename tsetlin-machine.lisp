(defclass tsetlin-machine ()
  ((num-clauses :initarg :num-clauses :accessor num-clauses)
   (num-features :initarg :num-features :accessor num-features)
   (threshold :initarg :threshold :accessor threshold)
   (weights :initarg :weights :accessor weights)
   (biases :initarg :biases :accessor biases)))

(defun tsetlin-machine-eval (machine input)
  (let ((sum 0))
    (dotimes (clause (num-clauses machine))
      (let ((clause-sum 0))
        (dotimes (feature (num-features machine))
          (when (and (aref input feature)
                     (aref (weights machine) clause feature))
            (incf clause-sum)))
        (when (>= clause-sum (threshold machine))
          (incf sum (aref (biases machine) clause)))))
    sum))

(defun tsetlin-machine-train (machine input target)
  (let ((output (tsetlin-machine-eval machine input))
        (error (- target (tsetlin-machine-eval machine input))))
    (when (/= output target)
      (dotimes (clause (num-clauses machine))
        (dotimes (feature (num-features machine))
          (when (and (aref input feature)
                     (aref (weights machine) clause feature))
            (setf (aref (weights machine) clause feature)
                  (if (> target output)
                      (min 1 (+ (aref (weights machine) clause feature) 1))
                      (max 0 (- (aref (weights machine) clause feature) 1)))))))
      (dotimes (clause (num-clauses machine))
        (setf (aref (biases machine) clause)
              (if (> target output)
                  (min 100 (+ (aref (biases machine) clause) 1))
                  (max -100 (- (aref (biases machine) clause) 1))))))
    error))

(defun make-tsetlin-machine (num-clauses num-features threshold)
  (let* ((weights (make-array `(,num-clauses ,num-features)
                               :initial-element 1))
         (biases (make-array num-clauses
                              :initial-element 0)))
    (make-instance 'tsetlin-machine
                   :num-clauses num-clauses
                   :num-features num-features
                   :threshold threshold
                   :weights weights
                   :biases biases)))

(defun tests ()
  (let ((machine (make-tsetlin-machine 3 4     
    (print (tsetlin-machine-eval machine #(1 0 1 0)))
    
    (print (tsetlin-machine-train machine #(1 0 1 0) 1))
    (print (tsetlin-machine-eval machine #(1 0 1 0)))

    (print (tsetlin-machine-train machine #(0 1 0 1) 0))
    (print (tsetlin-machine-eval machine #(0 1 0 1)))))
