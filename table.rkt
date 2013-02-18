#lang racket

; NOTE: Evaluating (void) returns void

(define table (make-hash))

(define put 
  (lambda (obj operator fn)
    (let ((internal (hash-ref table obj (void))))
      (cond ((void? internal)
             (let ((new-table (make-hash)))
               (hash-set! table obj new-table)
               (hash-set! new-table operator fn)))
            (else
             (hash-set! internal operator fn))))))

(define get
  (lambda (obj operator)
    (let ((internal (hash-ref table obj (void))))
      (cond ((void? internal) internal)
            (else (hash-ref internal operator (void)))))))

; this function is just for debugging purposes
(define peek
  (lambda ()
    (map
     (lambda (x) (cons (car x) (hash->list (cdr x)))) 
     (hash->list table))))

(provide put get peek)
