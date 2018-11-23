(define listofprime (list
                     2
                     3
                     5
                     7
                     11
                     13
                     17
                     19
                     23
                     29
                     31
                     37
                     41
                     43
                     47))

(define (prime vmin vmax) ; generates a list of prime numbers within range given
  (local [(define (raise n)
            (+ vmin n))
          (define (prime? n)
            (local [(define (using prime)
                      (if (= n prime)
                          true
                          (not (zero? (modulo n prime)))))]
              (andmap using listofprime)))]
    (rest (filter prime? (map raise (build-list vmax identity))))))

(define (twin lop n)
  (cond [(empty? (rest lop)) (string-append "Total " (number->string n) " twin primes")]
        [else
         (if (= (- (second lop) (first lop)) 2)
             (append (list (string-append (number->string (first lop)) ", " (number->string (second lop))) (twin (rest lop) (add1 n))))
             (twin (rest lop) n))]))

(define (twin-prime vmin vmax)
  (twin (prime vmin vmax) 0))
;; (twin-prime vmin vmax) to start
;; interp. vmin is value min and vmax is value max
;; Replace variables with a natural for desired range for finding twin primes
;; program runs from all natural in [0, 47!+1) and range can be raised by adding
;; sequential prime numbers into listofprimes, although it may make
;; make the runtime longer
