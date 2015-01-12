(module queues racket/base
  (provide enqueue dequeue make-empty-queue
           inner-dequeue (struct-out node) for-queue
           for-queue-terminating peek-queue
           make-queue)
  
  (struct node ([next #:mutable] [prev #:mutable] data) #:transparent)
  
  (struct queue
    ([head #:mutable]
     [tail #:mutable]) #:transparent)
  
  (define (make-empty-queue)
    (queue #f #f))
  
  (define-syntax make-queue
    (syntax-rules ()
      ((_ e)
       (enqueue e (make-empty-queue)))
      ((_ e e1 ...)
       (enqueue e (make-queue e1 ...)))))
  
  (define (enqueue elm queue)
    (if (queue-head queue) ; if we're not on the first element
        (let ([elm (node (queue-head queue) #f elm)])
          (set-node-prev! (queue-head queue) elm)
          (set-queue-head! queue elm))
        (let ([elm (node #f #f elm)]) ; if on the head
          (set-queue-head! queue elm) ; it's both head and tail
          (set-queue-tail! queue elm)))
    queue)
      
  (define (inner-dequeue queue)
    (let ([tail (queue-tail queue)])
      (if tail ; if not 0 element queue
          (begin0
            tail
            (set-queue-tail! queue (node-prev tail))
            (when (queue-tail queue)
              (set-node-next! (queue-tail queue) #f)))
          (begin
            (set-queue-head! queue #f)
            'queue-end))))
  
  ; performs fn on each element of the queue
  (define (for-queue queue fn)
    (let loop ([current-element (queue-head queue)])
      (when (node? current-element) 
        (fn (node-data current-element))
        (loop (node-next current-element)))))
  
  ; performs fn on each successive element of the queue until either the end is reached or fn returns #f
  (define (for-queue-terminating queue fn)
    (let loop ([current-element (queue-head queue)])
      (if (node? current-element)
          (let ([result (fn (node-data current-element))])
            (if result
                (loop (node-next current-element))
                #f))
          #t)))
  
  (define (peek-queue queue)
    (when (queue-head queue)
      (node-data (queue-head queue))))
  
  (define (dequeue queue)
    (let ([result (inner-dequeue queue)])
      (when (node? result)
        (node-data result)))))