(defpackage :event-glue-test
  (:use :cl :fiveam :event-glue)
  (:import-from #:event-glue
                #:dispatch-handlers
                #:dispatch-handler-names
                #:make-lookup-name)
  (:export #:run-tests))
(in-package :event-glue-test)

(def-suite event-glue :description "The main event-glue test suite.")
(in-suite event-glue)

(test make-dispatch
  "Tests making a dispatch object"
  (is (typep (make-dispatch) 'dispatch)))

(test make-event
  "Tests making an event object"
  (let ((event (event "click" :data "mystr" :meta '(:name "slappy" :age 27))))
    (is (typep event 'event))
    (is (string= (data event) "mystr"))
    (is (and (string= (gethash "name" (meta event)) "slappy")
             (= (gethash "age" (meta event)) 27)))))

(test binding
  "Tests binding/unbinding to a dispatcher"
  (let ((dispatch (make-dispatch))
        (fn (lambda (x) x)))
    (bind "click" fn :on dispatch)
    (is (= (length (gethash "click" (dispatch-handlers dispatch))) 1))
    (is (eq (unbind "click" fn :on dispatch) t))
    (is (eq (unbind "click" fn :on dispatch) nil))
    (is (= (length (gethash "click" (dispatch-handlers dispatch))) 0))))

(test binding-named
  "Tests named bindings/unbinding"
  (let* ((dispatch (make-dispatch))
         (fn (lambda (x) x))
         (name "my-open-event")
         (named (make-lookup-name "open" name)))
    (bind "open" fn :name name :on dispatch)
    (is (= (length (gethash "open" (dispatch-handlers dispatch))) 1))
    (is (eq fn (gethash named (dispatch-handler-names dispatch))))
    (unbind "open" name :on dispatch)
    (is (= (length (gethash "open" (dispatch-handlers dispatch))) 0))
    (is (eq nil (gethash named (dispatch-handler-names dispatch))))
    ;; make sure a named binding is REPLACED if given the same name/event
    (bind "fun" fn :name "test" :on dispatch)
    (bind "fun" fn :name "test" :on dispatch)
    (is (= 1 (length (gethash "fun" (dispatch-handlers dispatch)))))
    (bind "har" (lambda () nil) :name "test" :on dispatch)
    (bind "har" (lambda () nil) :name "test" :on dispatch)
    (is (= 1 (length (gethash "har" (dispatch-handlers dispatch)))))))

(test binding-multiple
  "Tests to make sure multiple handlers can be bound/unbound to one event"
  (let* ((dispatch (make-dispatch)))
    (bind "click" (lambda (x) x) :on dispatch)
    (bind "click" (lambda (y) y) :on dispatch)
    (bind "close" (lambda (z) z) :on dispatch)
    (is (= 2 (length (gethash "click" (dispatch-handlers dispatch)))))
    (is (= 1 (length (gethash "close" (dispatch-handlers dispatch)))))
    (unbind-all "click" :on dispatch)
    (is (= 0 (length (gethash "click" (dispatch-handlers dispatch)))))
    (is (= 1 (length (gethash "close" (dispatch-handlers dispatch)))))))

(test binding-once
  "Test our lovely bind-once function."
  (let ((dispatch (make-dispatch))
        (clicks 0))
    (bind-once "click" (lambda (ev) (incf clicks (data ev))) :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (is (= 3 clicks))))

(test binding-catch-all
  "Tests our catch-all event bindings."
  (let ((dispatch (make-dispatch))
        (num 0))
    (bind "click" (lambda (ev) (incf num) ev) :on dispatch)
    (bind :* (lambda (ev) (incf num) ev) :on dispatch)
    (trigger (event "click") :on dispatch)
    (is (= 2 num))
    (trigger (event "samson") :on dispatch)
    (trigger (event "howser") :on dispatch)
    (trigger (event "samson") :on dispatch)
    (trigger (event "howser") :on dispatch)
    (is (= 6 num))))

(test binding-catch-all-unbind-function
  "Test our catch-all bindings' unbind-function actually works."
  (let ((dispatch (make-dispatch))
        (num 0))
    (multiple-value-bind (_ unbind-fn)
        (bind :* (lambda (ev) (incf num) ev) :on dispatch)
      (declare (ignore _))
      (trigger (event "imteamleader") :on dispatch)
      (funcall unbind-fn)
      (trigger (event "ifanythingtheyshouldberewarded") :on dispatch)
      (is (= 1 num)))))

(test unbind-function
  "Test that we can unbind via our second return from bind."
  (let ((dispatch (make-dispatch))
        (clicks 0))
    (multiple-value-bind (_ unbind-fn)
        (bind "click" (lambda (ev) (incf clicks (data ev))) :on dispatch)
      (declare (ignore _))
      (trigger (event "click" :data 1) :on dispatch)
      (funcall unbind-fn)
      (trigger (event "click" :data 1) :on dispatch)
      (is (= 1 clicks)))))

(test unbind-once-function
  "Test that we can unbind via our second return from bind-once."
  (let ((dispatch (make-dispatch))
        (clicks 0))
    (multiple-value-bind (_ unbind-fn)
        (bind "click" (lambda (ev) (incf clicks (data ev))) :on dispatch)
      (declare (ignore _))
      (funcall unbind-fn)
      (trigger (event "click" :data 1) :on dispatch)
      (is (= 0 clicks)))))

(test trigger
  "Tests triggering of events (and lack thereof after unbinding)"
  (let* ((dispatch (make-dispatch))
         (c 0)
         (x 0)
         (y ""))
    (bind "click" (lambda (e)
                    (incf c)
                    (incf x (data e))) :on dispatch)
    (bind "click" (lambda (e)
                    (incf c)
                    (incf x (* 2 (data e)))) :on dispatch)
    (bind "fire" (lambda (e)
                   (incf c)
                   (setf y (concatenate 'string y (data e)))) :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (trigger (event "fire" :data "JETSON, YOU'RE FIRED.") :on dispatch)
    (unbind-all "click" :on dispatch)
    (unbind-all "fire" :on dispatch)
    (trigger (event "click" :data 3) :on dispatch)
    (trigger (event "fire" :data "IGNORE ME!!") :on dispatch)
    (is (= c 3))
    (is (= x 9))
    (is (string= y "JETSON, YOU'RE FIRED."))))

(test trigger-ordering
  "Tests that bindings fire in the order the were added."
  (let ((dispatch (make-dispatch))
        (num 6))
    (bind "click" (lambda (ev) (setf num (expt num 2)) ev) :on dispatch)
    (bind "click" (lambda (ev) (setf num (/ num 2)) ev) :on dispatch)
    (trigger (event "click") :on dispatch)
    (is (= 18 num))))

(test forwarding-simple
  "Tests simple dispatcher forwarding"
  (let* ((dispatch (make-dispatch))
         (hub (make-dispatch))
         (clicks 0))
    (bind "click" (lambda (x) (incf clicks) x) :on dispatch)
    (trigger (event "click") :on hub)
    (forward hub dispatch)
    (trigger (event "click") :on hub)
    (is (= clicks 1))))

(test forwarding-filter
  "Tests that dispatchers apply filters correctly when forwarding"
  (let ((dispatch (make-dispatch))
        (hub (make-dispatch))
        (clicks 0))
    (forward hub (lambda (ev)
                   (when (< (data ev) 4)
                     dispatch)))
    (bind "click" (lambda (ev) (incf clicks) ev) :on dispatch)
    (trigger (event "click" :data 2) :on hub)
    (trigger (event "click" :data 1) :on hub)
    (trigger (event "click" :data 0) :on hub)
    (trigger (event "click" :data 6) :on hub)
    (trigger (event "click" :data 4) :on hub)
    (trigger (event "click" :data 9) :on hub)
    (is (= clicks 3))))

(test forward-test
  "Test forwardsp"
  (let ((a (make-dispatch))
        (b (make-dispatch)))
    (is (eq nil (forwardsp a b)))
    (forward a b)
    (is (eq b (forwardsp a b)))
    (unforward a b)
    (is (eq nil (forwardsp a b)))))

(test unforward
  "Tests that a forward can be cleared."
  (let ((dispatch (make-dispatch))
        (hub (make-dispatch))
        (clicks 0))
    (bind "click" (lambda (x) (incf clicks) x) :on dispatch)
    (forward hub dispatch)
    (trigger (event "click") :on hub)
    (unforward hub dispatch)
    (trigger (event "click") :on hub)
    (is (= 1 clicks))))

(defun run-tests ()
  (run! 'event-glue))

