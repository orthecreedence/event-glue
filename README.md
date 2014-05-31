event-glue: simple eventing abstraction
=======================================
event-glue is a library that offers simple abstraction around event processing.
It's goal is to be compact, performant, extendable, and make no assumptions
about your eventing infrastructure. It currently has no dependencies.

It is used in [turtl-core](https://github.com/turtl/core) to provide the main
fabric of communication between various pieces of the app. It can be used
anywhere you need a generic event handling system.

## API/docs

- [dispatch (class)](#dispatch-class)
- [\*dispatch\* (object)](#dispatch-object-of-type-dispatch)
- [make-dispatch (function)](#make-dispatch-function)
- [forward (function)](#forward-function)
- [forwardsp (function)](#forwardsp-function)
- [unforward (function)](#unforward-function)
- [event (class)](#event-class)
  - [ev (accessor)](#ev-accessor)
  - [data (accessor)](#data-accessor)
  - [meta (accessor)](#meta-accessor)
- [event (function)](#event-function)
- [bind (function)](#bind-function)
- [bind-once (function)](#bind-once-function)
- [unbind (function)](#unbind-function)
- [unbind-all (function)](#unbind-all-function)
- [wipe (function)](#wipe-function)
- [trigger (function)](#trigger-function)

### dispatch (class)
`dispatch` is an opaque class. It is an object that matches events to event
bindings. All event bindings live in a dispatcher, and all events that are
triggred are triggered on a dispatcher. It is the backbone of event-glue and all
events flow through a dispatcher.

It has no public accessors, but is exported so you can expand it within your app
to add things like a synchronized queue.

Note that dispatchers can feed events into each other, either by passing all
events or using a function to filter them (see [forward](#forward-function)).

### \*dispatch\* (object of type dispatch)
This is the global *default* event dispatcher. If you use the [bind](#bind-function)
or [trigger](#trigger-function) functions without specifying the `:dispatch`
keyword, `*dispatch*` is used.

It is created on load via `defvar` meaning that subsequent loads will preserve
the object. It is exported so that in the event you want to extend the [dispatch class](#dispatch-class),
you can create your own dispatch object and set it into `event-glue:*dispatch*`
and everything will use your extended class by default.

### make-dispatch (function)
```lisp
(defun make-dispatch ())
  => dispatch
```

This function creates a new dispatcher.

### forward (function)
```lisp
(defun forward (from to-or-function))
  => nil
```

Sets up a *forward* between two dispatchers. `from` is the [dispatcher](#dispatch-class)
that we want to forward events from, `to-or-function` can be either

- another dispatch object, in which *all* events triggered on `from` will flow
throw, unfettered, to the `to` dispatcher.
- a function which has one argument (the event being triggered) that returns
either `nil` (meaning don't forward this event) or a [dispatcher](#dispatch-class)
object, in which case the event will be triggered on the returned dispatcher.

Note that if A forward to B, triggering an event on A will fire A's handlers
before the events are forwarded to B.

Example:
```lisp
(let* ((all-events (make-dispatch))
       (click-events (make-dispatch))
       (hub (make-dispatch)))
  ;; all-events will get *all* events that hub gets
  (forward hub all-events)
  ;; click-events will only get events where the event name is "click"
  (forward hub
    (lambda (event)
      (when (string= (ev event) "click")
        click-events))))
```

### forwardsp (function)
```lisp
(defun forwardsp (from to-or-function))
  => to-or-function/nil
```
Test if `from` forwards to `to-or-function`

### unforward (function)
```lisp
(defun unforward (from to-or-function))
  => nil
```
Undoes a forward created by [forward](#forward-function). The `to-or-function`
object must be `eq` to the one used to set up the forward, or it will not be
removed.

Example:
```lisp
(let ((main (make-dispatch))
      (hub (make-dispatch)))
  (forward hub main)
  (unforward hub main))
```

### event (class)
This class holds information about an event. This consists of the event's name,
the event's data (which is an arbitrary object), and any metadata associated
with the event. The class is public, allowing you to extend it and add any extra
fields required to it (such as a UUID field).

Events are created using the [event function](#event-function), and
are generally passed to [trigger](#trigger-function) (or they could be
serialized and sent off somewhere).

Events have three public accessors:

#### ev (accessor)
The event's name, generally a string.

#### data (accessor)
The event's data. This can be a number, a string, a list...anything you want.
There are no restrictions on the data an event can hold.

#### meta (accessor)
This is a hash-table that consists of information about the event that doesn't
necesarily fit into the event's data. For instance, you may want to mark what
source an event came from in you app, but that information doesn't pertain to
the event's data payload.

### event (function)
```lisp
(defun event (name &key data meta (type 'event)))
  => event
```

Create a new event object with the given name, data, and meta. `name` is
generally a string, although if you wish you can use symbols or keywords as
well. `data` can be any object you want to attach to the event. `meta` can
be either a hash table or a plist (if plist, key names are `string-downcase`ed)
that gives extra information about the event.

`event` also takes a `:type` keyword (which defaults to [event](#event-class))
that allows you to create an event of your own type (for instance, you may
extend `event` and use `event` to create instances of your object).

Example:
```lisp
(event "click" :data '(:button-id 10) :meta '(:mouse-click t))

;; extension example
(defclass my-event (event) ())
(event "burnourourcorruptcapitalistsystemdowntotheground" :type 'my-event)
```

### bind (function)
```lisp
(defun bind (event-name function &key name (dispatch *dispatch*)))
  => function, unbind-function
```
Bind `function` to the given `event-name` on the `dispatch` object. This means
that whenever [trigger](#trigger-function) is called on `dispatch` with that
`event-name`, `function` will be called.

`function` must take one argument, which will be the event object that was
triggered.

The `:name` keyword allows you to "name" a binding. This is useful when you
want to bind an event to anonymous function but you don't want to keep a
reference to the function around if you need to unbind (which you'd normally
have to do, see [unbind](#unbind-function)). Instead, you can name a binding
and then unbind that function with the same name later.

Returns the passed function and also a second function of 0 args that, when
called, [unbinds](#unbind-function) the event.

Examples:
```lisp
;; bind the click-handler function to the "click" event on the global dispatch
(bind "click" 'click-handler)

;; create our own dispatch and bind to the "close" event on it.
(let ((my-dispatch (make-dispatch)))
  (bind "close" (lambda (event) (format t "closed: ~a~%" event)) :dispatch my-dispatch))

;; use named events to unbind an anonymous lambda
(bind "fire" (lambda (ev) (format t "JETSON, ...")) :name "fire:jetson")
;...
(unbind "fire" "fire:jetson")
```

### bind-once (function)
```lisp
(defun bind-once (event-name function &key name (dispatch *dispatch*)))
  => function, unbind-function
```
Almost exactly like [bind](#bind-function), except that the binding only lasts
for one triggering (or until it's removed).

Returns the passed function and also a second function of 0 args that, when
called, [unbinds](#unbind-function) the event.

Example:
```lisp
(bind-once "call" (lambda (ev) (format t "call from ~a~%" (data ev))))
(trigger (event "call") :data "sally")  ; hi, sally
(trigger (event "call") :data "frank")  ; frank's call is ignored
```

### unbind (function)
```lisp
(defun unbind (event-name function-or-name &key (dispatch *dispatch*)))
  => nil
```
Unbind `function-or-name` from the `event-name` on the `dispatch` object. This
is essentially the opposite of [bind](#bind-function), allowing us to no longer
have the given function (or binding name) triggered when the given `event-name`
is triggered.

Example:
```lisp
;; bind/unbind using a function
(let ((my-click-fn (lambda (event) (format t "clicked button: ~a~%" (data event)))))
  (bind "click" my-click-fn)
  (unbind "click" my-click-fn))

;; bind/unbind using a named binding
(bind "click" (lamdbda (event) (format t "clicked: ~a~%" (data event))) :name "click:format")
(unbind "click" "click:format")
```

### unbind-all (function)
```lisp
(defun unbind-all (event-name &key (dispatch *dispatch*)))
  => nil
```
Unbind all events of type `event-name` on the dispatcher.

Example:
```lisp
(bind "click" 'my-click-handler)
(bind "click" 'my-other-click-handler)
(bind "throw" 'ball-was-thrown)
(unbind-all "click")
;; *dispatch* now only contains a handler for "throw"
```

### wipe (function)
```lisp
(defun wipe (&key preserve-forwards (dispatch *dispatch*)))
  => nil
```
Wipe out a dispatch object. This includes all handlers of all types.

If `:preserve-forwards` is true, then the dispatch object will maintain its
relationships to other dispatch objects. Otherwise, forwards are removed as well
(see [forward](#forward-function)).

### trigger (function)
```lisp
(defun trigger (event &key (dispatch *dispatch*)))
  => nil
```
Finally, `trigger` is what we use to actually fire events.

Examples:
```lisp
(bind "click" (lambda (event) (format t "clicked: ~a~%" (data event))))
(bind "click" (lambda (event) (format t "click!~%")))
(trigger (event "click" :data 'red-button))
```

## Tests

Load up the `event-glue-test` system and run `(event-glue-test:run-tests)`.

## License

MIT.

