;;;; State

;;;; Events

(defclass event ()
  ((state :reader what)
   (locus :reader where)))

(defmethod make-event ((event event) state locus)
  (make-instance 'event )

;;;; History

(defclass history ()
  ((event-type :reader event-type)
   (events   :accessor events)))

(defmethod record-event ((history history) event)
  (assert (typep event (event-type history)))
  (push event (events history)))

(defmethod events-since ((history history) event)
  (nreverse
   (loop
      for event in (events history)
      collect event
      until (equal event last-event))))

(defmethod now ((history history))
  (state (first (events history))))

(defmethod here ((history history))
  (locus (first (events history))))


;;;; Creatures

(defclass creature (event)
  ((health :reader health)))

(defun make)

(defmethod wound-creature ((creature-history creature-history))
  (record-event (make-event 'wound-creature 
  
  (decf (now (health creature)))

(defun creature-alive-p ((creature creature))
  (< 0 (health creature)))
