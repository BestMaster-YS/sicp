(load "../homework/util.scm")

;;Implementing The agenda

(define (make-time-segment time queue) (cons time queue))
(define (segment-time segment) (car segment))
(define (segment-queue segment) (cdr segment))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))



;; Representing wire

(define (make-wire)
  (let ((signal-value 0)
        (action-precedure '()))
    (define (set-signal! new-value)
      (if (not (= new-value signal-value))
          (begin (set! signal-value new-value)
                 (call-each action-precedure))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-precedure
            (cons proc action-precedure))
      (proc))
    (define (dispatch op)
      (cond ((eq? op 'get-signal!) signal-value)
            ((eq? op 'set-signal!) set-signal!)
            ((eq? op 'add-action!) accept-action-procedure!)
            (else
             (error "Unknown operation: WIRE" op))))
    dispatch))

(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (get-signal! wire) (wire 'get-signal!))
(define (add-action! wire action-precedure)
  ((wire 'add-action!) action-precedure))

;; A simple simulation

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value ")
                 (display (get-signal! wire)))))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define or-gate-delay 3)
(define and-gate-delay 5)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b in sum out)
  (let ((s (make-wire))
        (c2 (make-wire))
        (c1 (make-wire)))
    (half-adder b in s c1)
    (half-adder a s sum c2)
    (or-gate c2 c1 out)
    'ok))

;; primitive function boxes

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal! input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)


(define (and-gate in1 in2 out)
  (define (and-action-procudure)
    (let ((new-value (logical-and (get-signal! in1) (get-signal! in2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! out new-value)))))
  (add-action! in1 and-action-procudure)
  (add-action! in2 and-action-procudure)
  'ok)


(define (logical-not in)
  (cond ((= in 0) 1)
        ((= in 1) 0)
        (else
         (error "Invalid signal" in))))

(define (logical-and in1 in2)
  (cond ((and (= in1 1) (= in2 1)) 1)
        ((or (not (or (= in1 1) (= in1 0)))
             (not (or (= in2 1) (= in2 0))))
         (error "Invalid sigal" in1 in2))
        (else
         0)))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))


(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;test

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(display the-agenda)

(set-signal! input-1 1)

(propagate)

(set-signal! input-2 1)

(propagate)
