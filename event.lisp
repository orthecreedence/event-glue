(in-package :event-glue)

(defclass dispatch ()
  ((handlers :accessor dispatch-handlers :initform (make-hash-table :test #'equal)
     :documentation "Holds the dispatcher's event handlers (event-name -> fn).")
   (handler-names :accessor dispatch-handler-names :initform (make-hash-table :test #'equal)
     :documentation "Holds named event name -> fn bindings for easy lookups.")
   (forwards :accessor dispatch-forwards :initarg :forwards :initform nil
     :documentation "Holds any other dispatchers this dispatcher forwards to."))
  (:documentation
    "The dispatch class is what event listeners bind to. Events flow throw it."))

(defvar *dispatch* (make-instance 'dispatch)
  "Our global dispatch handler. This is the default handler used if a dispatcher
   is not specified in event operations.")

(defun make-dispatch ()
  "Make a dispatcher."
  (make-instance 'dispatch))

(defun forward (from to-or-function)
  "Forward events from one dispatcher to another. If the second dispatcher is
   given as a function, that function must return either another dispatcher or
   nil. This lets you forward specific events at runtime based on data within
   the event."
  (push to-or-function (dispatch-forwards from)))

(defun forwardsp (from to-or-function)
  "Determine if the given from -> to forward is active. Returns either
   to-or-function or nil."
  (find to-or-function (dispatch-forwards from)))

(defun unforward (from to-or-function)
  "Undo a forward created by forward."
  (setf (dispatch-forwards from) (remove to-or-function (dispatch-forwards from) :test 'eq)))

(defclass event ()
  ((ev :accessor ev :initarg :ev :initform nil
     :documentation "Holds the event's name.")
   (data :accessor data :initarg :data :initform nil
     :documentation "Arbitrary data attached to the event. Usually a set of args.")
   (meta :accessor meta :initarg :meta :initform (make-hash-table :test #'equal)
     :documentation "Any top-level meta associated with the event, used to describe it."))
  (:documentation
    "Describes an event and any data it holds."))

(defmethod print-object ((event event) s)
  (print-unreadable-object (event s :type t :identity t)
    (format s "~_data: ~a " (data event))
    (format s "~_meta: ~s" (if (hash-table-p (meta event))
                               (hash-table-count (meta event))
                               0))))

(defun event (name &key data meta (type 'event))
  "Easy wrapper for creating a standard event object. Meta is a plist of
   optional data to set (top-level) into the event object."
  (let ((event (make-instance type :ev name :data data)))
    (cond ((typep meta 'hash-table)
           (setf (meta event) meta))
          ((consp meta)
           (loop for (k v) on meta by #'cddr do
             (setf (gethash (string-downcase (string k)) (meta event)) v))))
    event))

(defun make-lookup-name (event-name name)
  "Standardizes the naming convention for named event names."
  (concatenate 'string
               (string event-name)
               "@"
               (string name)))

(defun bind (event-name function &key name (dispatch *dispatch*))
  "Bind a function to an event. Optionally allows naming the binding so it can
   be removed later on without the reference to the bound function."
  (let* ((handlers (dispatch-handlers dispatch))
         (event-handlers (gethash event-name handlers)))
    (unless (find function event-handlers :test 'eq)
      ;; append instead of push here. this means when the event fires, the
      ;; bindings fire in the order added.
      (setf event-handlers (append event-handlers (list function)))
      (setf (gethash event-name handlers) event-handlers))
    (when name
      (setf (gethash (make-lookup-name event-name name) (dispatch-handler-names dispatch)) function))
    ;; return the original function AND a function that unbinds the event if
    ;; called
    (values function
            (lambda () (unbind event-name function :dispatch dispatch)))))

(defun bind-once (event-name function &key name (dispatch *dispatch*))
  "Bind a function to an event, but clear the binding out once the event has
   been triggered once."
  (let ((wrapped-function nil))
    ;; use setf here so we can access wrapped-function from within itself.
    (setf wrapped-function 
          (lambda (event)
            (unbind event-name wrapped-function :dispatch dispatch)
            (funcall function event)))
    ;; now just call bind as normal
    (bind event-name wrapped-function :name name :dispatch dispatch)))

(defun unbind (event-name function-or-name &key (dispatch *dispatch*))
  "Unbind an event/function pair. If function-or-name contains a non-function
   value, the value is used in a name lookup instead. This allows removing an
   event/function binding by its name (as specified by :name in the bind
   function) which can be nice when the original lambda is no longer around."
  (let ((function (if (functionp function-or-name)
                      function-or-name
                      (gethash (make-lookup-name event-name function-or-name) (dispatch-handler-names dispatch))))
        (handlers (dispatch-handlers dispatch)))
    (when function
      ;; clean up the name binding
      (unless (functionp function-or-name)
        (remhash (make-lookup-name event-name function-or-name) (dispatch-handler-names dispatch)))
      (let ((size (length (gethash event-name handlers)))
            (removed (remove function (gethash event-name handlers))))
        (setf (gethash event-name handlers) removed)
        (< (length (gethash event-name handlers)) size)))))

(defun unbind-all (event-name &key (dispatch *dispatch*))
  "Unbind all handlers for the given event name."
  (setf (gethash event-name (dispatch-handlers dispatch)) nil))

(defun wipe (&key preserve-forwards (dispatch *dispatch*))
  "Wipe out all handlers for a dispatch object."
  (setf (dispatch-handlers dispatch) (make-hash-table :test #'equal)
        (dispatch-handler-names dispatch) (make-hash-table :test #'equal))
  (unless preserve-forwards
    (setf (dispatch-forwards dispatch) nil)))

(defun trigger (event &key (dispatch *dispatch*))
  "Trigger en event."
  (let* ((event-name (ev event))
         (handlers (gethash event-name (dispatch-handlers dispatch)))
         ;; grab catch-all bindings (fired for every event)
         (catch-all (gethash :* (dispatch-handlers dispatch)))
         (handlers (if catch-all
                       (append handlers catch-all)
                       handlers))
         (forwards (dispatch-forwards dispatch)))
    (dolist (fn handlers)
      (funcall fn event))
    (when forwards
      (dolist (forward-to (reverse forwards))
        (cond ((typep forward-to 'dispatch)
               (trigger event :dispatch forward-to))
              ((typep forward-to '(or symbol function))
               (let ((conditional-dispatch (funcall forward-to event)))
                 (when conditional-dispatch
                   (trigger event :dispatch conditional-dispatch)))))))))

