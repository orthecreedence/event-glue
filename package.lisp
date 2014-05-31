(defpackage :event-glue
  (:use :cl)
  (:export #:dispatch
           #:*dispatch*
           #:make-dispatch
           #:forward
           #:forwardsp
           #:unforward
           #:event    ; event (class)
           #:ev
           #:data
           #:meta
           #:event    ; event (function)
           #:bind
           #:bind-once
           #:unbind
           #:unbind-all
           #:wipe
           #:trigger)
  (:nicknames :ev))

