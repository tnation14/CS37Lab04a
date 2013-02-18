#lang racket

;; A complete working implementation of the generic
;; number package from SICP section 2.5.1 pp. 189-192.

(require "table.rkt")

;; tag implementation
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_idx_2368

(define attach-tag
  (lambda (type-tag contents)
    (cons type-tag contents)))

(define type-tag
  (lambda (datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum))))

(define contents
  (lambda (datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum))))

;; apply-generic
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_idx_2462

(define apply-generic 
  (lambda (op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error
             "No method for these types -- APPLY-GENERIC"
             (list op type-tags)))))))

;; now to the big (scheme, rational, complex) generic package
;; follow from section 2.5
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-18.html

(define add (lambda (x y) (apply-generic 'add x y)))
(define sub (lambda (x y) (apply-generic 'sub x y)))
(define mul (lambda (x y) (apply-generic 'mul x y)))
(define div (lambda (x y) (apply-generic 'div x y)))

(define install-scheme-number-package
  (lambda ()
    (define tag 
      (lambda (x)
        (attach-tag 'scheme-number x)))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (display "Scheme number package installed\n")))

(define make-scheme-number
  (lambda (n)
    ((get 'make 'scheme-number) n)))

(define install-rational-package
  (lambda ()
    
    ;; some internal definitions
    (define gcd
      (lambda (a b)
        (if (= b 0)
            a
            (gcd b (remainder a b)))))
    (define numer (lambda (x) (car x)))
    (define denom (lambda (x) (cdr x)))
    (define make-rat 
      (lambda (n d)
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
    (define add-rat 
      (lambda (x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y)))))
    (define sub-rat 
      (lambda (x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y)))))
    (define mul-rat
      (lambda (x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y)))))
    (define div-rat 
      (lambda (x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y)))))
    
    ;; interface to rest of the system
    (define tag 
      (lambda (x)
        (attach-tag 'rational x)))
    
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    (display "Rational number package installed\n")))


(define make-rational 
  (lambda (n d)
    ((get 'make 'rational) n d)))

(define install-complex-package
  (lambda ()

    ;; internal defintion for both sub-packages
    (define square (lambda (x) (* x x)))
  
    (define install-rectangular-package
      (lambda () 
        ;; internal procedures for rectangular
        (define real-part (lambda (z) (car z)))
        (define imag-part (lambda (z) (cdr z)))
        (define make-from-real-imag (lambda (x y) (cons x y)))
        (define magnitude 
          (lambda (z)
            (sqrt (+ (square (real-part z))
                     (square (imag-part z))))))
        (define angle 
          (lambda (z)
            (atan (imag-part z) (real-part z))))
        (define make-from-mag-ang
          (lambda (r a) 
            (cons (* r (cos a)) (* r (sin a)))))
        
        ;; interface to the rest of the system
        (define tag (lambda (x) (attach-tag 'rectangular x)))
        
        (put 'real-part '(rectangular) real-part)
        (put 'imag-part '(rectangular) imag-part)
        (put 'magnitude '(rectangular) magnitude)
        (put 'angle '(rectangular) angle)
        (put 'make-from-real-imag 'rectangular
             (lambda (x y) (tag (make-from-real-imag x y))))
        (put 'make-from-mag-ang 'rectangular
             (lambda (r a) (tag (make-from-mag-ang r a))))))
      
      (define install-polar-package
        (lambda ()
          ;; internal procedures for polar
          (define magnitude (lambda (z) (car z)))
          (define angle (lambda (z) (cdr z)))
          (define make-from-mag-ang (lambda (r a) (cons r a)))
          (define real-part 
            (lambda (z)
              (* (magnitude z) (cos (angle z)))))
          (define imag-part 
            (lambda (z)
              (* (magnitude z) (sin (angle z)))))
          (define make-from-real-imag 
            (lambda (x y) 
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
          
          ;; interface to the rest of the system
          (define tag (lambda (x) (attach-tag 'polar x)))
          (put 'real-part '(polar) real-part)
          (put 'imag-part '(polar) imag-part)
          (put 'magnitude '(polar) magnitude)
          (put 'angle '(polar) angle)
          (put 'make-from-real-imag 'polar
               (lambda (x y) (tag (make-from-real-imag x y))))
          (put 'make-from-mag-ang 'polar
               (lambda (r a) (tag (make-from-mag-ang r a))))))
  
    ;; Generic selectors    
    (define real-part (lambda (z) (apply-generic 'real-part z)))
    (define imag-part (lambda (z) (apply-generic 'imag-part z)))
    (define magnitude (lambda (z) (apply-generic 'magnitude z)))
    (define angle (lambda (z) (apply-generic 'angle z)))
        
    ;; Constructors for complex numbers
    (define make-from-real-imag
      (lambda (x y)
        ((get 'make-from-real-imag 'rectangular) x y)))
    
    (define make-from-mag-ang 
      (lambda (r a)
        ((get 'make-from-mag-ang 'polar) r a)))
    
    ;; Now the new stuff
    
    ;; internal procedures
    (define add-complex 
      (lambda (z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2)))))
      
    (define sub-complex 
      (lambda (z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2)))))
    
    (define mul-complex
      (lambda (z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2)))))

    (define div-complex 
      (lambda (z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2)))))

    ;; interface to rest of the system
    (define tag (lambda (z) (attach-tag 'complex z)))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    
    ;; need to install both packages if you want them to work
    (install-rectangular-package)
    (install-polar-package)
    (display "Complex package installed\n")))    

(define make-complex-from-real-imag 
  (lambda (x y)
    ((get 'make-from-real-imag 'complex) x y)))

(define make-complex-from-mag-ang 
  (lambda (r a)
    ((get 'make-from-mag-ang 'complex) r a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; install scheme number package
(install-scheme-number-package)

;;define a few scheme numbers
(define seven (make-scheme-number 7))
(define ten (make-scheme-number 10))
(define five (make-scheme-number 5))

;; install complex package
(install-complex-package)

;;define a few complex numbers from magnitude, angle
(define a30 (make-complex-from-mag-ang  2 (/ pi 6)))
(define a45 (make-complex-from-mag-ang  (* 9/5 pi) (/ pi 4)))
(define a60 (make-complex-from-mag-ang  4 (/ pi 3)))

;;define a few complex numbers from real, imaginary
(define b30 (make-complex-from-real-imag (sqrt 3) 1))
(define b45 (make-complex-from-real-imag 4 4))
(define b60 (make-complex-from-real-imag 2 (* 2 (sqrt 3))))



;; install rational package
(install-rational-package)

;;define a few rational numbers
(define pct75 (make-rational 3 4))
(define tenr (make-rational 10 1))
(define pct10 (make-rational 1 10))


;; 1. Exercise 2.77
;; Read the description in the lab writeup.
;; You will need this code:

;: (put 'real-part '(complex) real-part)
;: (put 'imag-part '(complex) imag-part)
;: (put 'magnitude '(complex) magnitude)
;: (put 'angle '(complex) angle)

;; You may put your answer to 2.77 anywhere in the file, but show that
;; it works below.




;; 2. Exercise 2.79

;; You may put your answer to 2.79 anywhere in the file, but show that
;; it works below.