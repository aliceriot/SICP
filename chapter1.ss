;; just my notes on chapter 1
;; and things to try on the interpreter

(define (square x)(* x x))

;; I don't really know how to specify, in a rigorous way, how this differs from

(define square
  (lambda (x)
    (* x x)))

;; which performs the same way
;; I suspect SICP will teach me!

(define (sum-of-squares x y)
  (+ (square x)(square y)))

;; control flow

(define (abs x)
  (cond ((< x 0)(- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y)(= x y)))

;; exercise 1.1

10 -> 10
(+ 5 3 4) -> 12
(define a 3) -> none
(define b (+ a 1)) -> none (b = 4 though)
(if (and (> a b)(< b (* a b)))
  b
  a) -> a
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) -> 16
(+ 2 (if (> b a) b a)) -> 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -l))
   (+ a 1))

;; exercise 1.2
(/ (+ 5
      (+ (/ 1 2)
         (- 2
            (- 3
               (+ 6
                  (/ 1 3))))))
   (* 3
      (* (- 6 2)
         (- 2 7))))

;; bleccchhh

;; exercise 1.3
(define (sum-large-two x y z)
  (cond ((and (> x z)(> y z))
         (sum-of-squares x y))
        ((and (> x y)(> z y))
         (sum-of-squares x z))
        ((and (> z x)(> y x))
         (sum-of-squares z y))))

;; exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; if b is greater than zero, add b to a
;; else, subtract (add abs value, - - b) from a

;;exercise 1.5
(define (p)(p))
(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))

;; normal vs applicative order
;; I think normal order will loop forever, while applicative order will return 0 (x)


;; trying something...
;; can we recur without lambda?

(define (sum lat)
  (cond ((null? lat) 0)
        (else
          (+ (car lat)(sum (cdr lat))))))

;; we can! I have no idea what lambda does.

;; square root example
;; actually my improve version

(define (squarert x)
  (sqrt-iter 1.0 1.0 x))

(define (sqrt-iter guess difference x)
  (if (< (/ difference x) 0.000001)
    guess
    (sqrt-iter (improve guess x) (abs (- (improve guess x) guess)) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; exercise 1.6

;; Here someone has come up with a new form of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3)'("yes")'("no"))

;; the answer the book seems to leaning towards is that the normal if clause evaluates the 
;; 'then' statement IFF the predicate is false
;; but chez scheme doesn't seem to work that way?
;; I write functions with recursive calls using cond where the predicate is not false
;; and the 'then' statement isn't evaluated...
;; confused

;; exercise 1.7

;; why is our good-enough? test bad for both very large and very small numbers
;; we test whether the difference between the square of our guess and the number is smaller 
;; than 0.01 or something, so this won't work very well for numbers where 

;; see above for the code for this one

;; exercise 1.8

;; now we'd like to adapt our square root finder to cube root

;; all we need to change is the improve guess!
(define (cuberoot x)
  (cbrt-iter 1.0 1.0 x))

(define (cbrt-iter guess difference x)
  (if (< (/ difference x) 0.000001)
    guess
    (cbrt-iter (improve guess x) (abs (- (improve guess x) guess)) x)))

(define improve 
  (lambda (guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; it works!

;; section 1.2

;; exercise 1.9
;; here we're concerned with the differences between iterative and recursive computing
;; key difference is *deferred procedures*
;; a deferred/recursive mode would be something like:
(define factorial
  (lambda (n)
    (cond ((eq? n 1) 1)
          (else (* n (factorial (- n 1)))))))

;; the answer comes from evaluating a chain of deferred computation that we build stepwise
;; something like:

(factorial 7)
(* 7 (factorial 6))
(* 7 (* 6 (factorial 5)))
(* 7 (* 6 (* 5 (factorial 4))))
(* 7 (* 6 (* 5 (* 4 (factorial 3)))))
(* 7 (* 6 (* 5 (* 4 (* 3 (factorial 2))))))
(* 7 (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1)))))))
(* 7 (* 6 (* 5 (* 4 (* 3 (* 2 (1)))))))
(* 7 (* 6 (* 5 (* 4 (* 3 2)))))
(* 7 (* 6 (* 5 (* 4 6))))
(* 7 (* 6 (* 5 24)))
(* 7 (* 6 120))
(* 7 720)
5040

;; so we defer computations as we go, and then we evaluate them from the inside out to 
;; construct our answer

;; this is very different (and dependent on having a stack) than a method where a function
;; has a bunch of state variables, and resuming computation at any specific point is just 
;; a matter of specifying the correct state variables.

;; exercise 1.9 asks us to compare the following:
(define (plus a b)
  (if (= a 0)
    b
    (add1 (plus (sub1 a) b))))

(define (plus a b)
  (if (= a 0)
    b
    (plus (sub1 a)(add1 b))))

;; the first is recursive - it relies on deferring the evaluating of add1 until the (= a 0)
;; clause is met
;; the other relies on state variables



;; exercise 1.10

;; ackermann's function (we did this already in the little schemer)
(define A
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0)(* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)(A x (- y 1)))))))

;; book wants us to think about the follwing:
(define (f n)(A 0 n)) ;; returns (* 2 n)
(define (g n)(A 1 n)) ;; returns (* 2 (A x (- y 1)))
(define (h n)(A 2 n)) ;; returns a longer thing...
(define (k n)(* 5 n n)) ;; returns 5*n^2


;; cool! ackermann's function is a nifty lil' guy

;; thinkin bout fibo

(define fib
  (lambda (n)
    (cond ((eq? n 0) 0)
          ((eq? n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2)))))))

;; the above procedure is mathematically correct, but can get very slow
;; once we're looking for a really big number we generate a huge number of redundant 
;; recursive calls

;; you can show that the number of end points on the tree-recursive diagram for this
;; (i.e. the number of times we get to (fib 1) or (fib 0)) is precisely equal to 
;; (fib(n+1)) for (fib n). That's terrible! esp. b/c the value of (fib n) grows exponentiall
;; with n!

;; for a tree recursive process like this, in general, the number of steps required is 
;; proportional to the # nodes, but the space required is proportional to the depth of 
;; the tree

;; the above method for fibonacci is a properly recursive method, but we don't like
;; how about an iterative method?

(define fib2
  (lambda (n)
    (letrec ((fib-iter
               (lambda (a b count)
                 (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1))))))
      (fib-iter 1 0 n))))

;; much faster! this grows linearly with input size (n) rather than exponentially
;; calculating (fib2 1000) was almost instantaneous, whereas I had to crtl-c on (fib 1000)

;; the quicksort algorithm is a tree-recursive process, is it not?
                                
;; ok, can't make it work as one function...

(define biggerThan
  (lambda (a lat)
    (cond
      ((null? lat)(quote()))
      ((< a (car lat))
       (cons (car lat)
             (biggerThan a (cdr lat))))
      (else
        (biggerThan a (cdr lat))))))

(define smallerThan
  (lambda (a lat)
    (cond
      ((null? lat)(quote()))
      ((>= a (car lat))
       (cons (car lat)
             (smallerThan a (cdr lat))))
      (else
        (smallerThan a (cdr lat))))))
                   
(define shitty-quicksort
  (lambda (lat)
    (cond
      ((null? lat)(quote()))
      (else
        (build (shitty-quicksort (smallerThan (car lat)(cdr lat)))
              (build (car lat)
                    (shitty-quicksort (biggerThan (car lat)(cdr lat)))))))))

;; well, I made shitty quicksort work, cannot get it to work in one function
;; giving up

;; Trying to implement this...much easier in haskell
;; scheme's syntax is more straightforward, in the sense that there are far fewer rules
;; but haskell's is much more expressive
;; anyway,

;; change counting example
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((eq? amount 0) 1)
        ((or (< amount 0)(= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((eq? kinds-of-coins 1) 1)       
        ((eq? kinds-of-coins 2) 5)
        ((eq? kinds-of-coins 3) 10)
        ((eq? kinds-of-coins 4) 25)
        ((eq? kinds-of-coins 5) 50)))

;; exercise 1.11

;; tree-recursive algo, very slow
(define eff
  (lambda (n)
    (cond ((< n 3) n)
          (else
            (+ (eff (- n 1))
               (* 2 (eff (- n 2)))
               (* 3 (eff (- n 3))))))))

;; iterative algo
(define eff2
  (lambda (n)
    (letrec
      ((eff-iter (lambda (a b c count)
                   (cond
                     ((eq? count 2) a)
                     (else
                       (eff-iter (+ a (* 2 b)(* 3 c))
                                 a
                                 b
                                 (- count 1)))))))
      (cond ((< n 3) n)
            (else
              (eff-iter 2 1 0 n))))))
                                    
;; again, way faster! (eff 100) is waaay slow, (eff2 100) almost instantaneous

;; get pascal triangle nums for arbitrary indexes
;; this is for exercise 1.12
(define pascal
  (lambda (row col)
    (cond ((eq? row 1) 1)
          ((eq? row col) 1)
          ((eq? col 1) 1)
          (else (+ (pascal (sub1 row)(sub1 col))
                   (pascal (sub1 row) col))))))

;; exercise 1.13 is a proof! I did it on paper

;; exercise 1.14 asks you to draw a thing, I also did that on paper


;; exercise 1.15

(define (cube x)(* x x x))

(define (p x)(- (* 3 x)(* 4 (cube x))))

(define (sine argnle)
  (if (not (> (abs arngle) 0.1))
    arngle
    (p (sine (/ arngle 3.0)))))

;; a 

(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (p (sine (/ 4.05 3.0))))
(p (p (p (sine (/ 1.3499999 3.0)))))
(p (p (p (p (sine (/ 0.4499999 3.0))))))
(p (p (p (p (p (sine (/ 0.15 3.0)))))))

;; I think it will stop there, since
(/ 0.15 3.0) -> 0.0499999
;; and this is less than our 0.1 threshold
;; so that means that we can toss that back up to p

;; I'm not really sure, but to me it looks like the number of steps is approximately
;; 1/2 n, where we're finding (sine n)
;; the space requirement seems to grow at roughly the same rate

;; the website says this is logarithmic, specifically 0(log_3(n))

;; 1.2.4, exponentiation

(define exponent
  (lambda (b n)
    (if (= n 0)
      1
      (* b (expt b (- n 1))))))

;; or

(define exponent2
  (lambda (b n)
    (letrec ((expt-iter
               (lambda (counter product)
                 (if (= counter 0)
                   product
                   (expt-iter (sub1 counter)
                              (* b product))))))
      (expt-iter n 1))))

;; niftier way

(define fast-expt
  (lambda (b n)
    (cond ((eq? n 0) 1)
          ((even? n)(square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1)))))))

;; exercise 1.16

(define (expo-iter a b n)
  (cond ((eq? n 0) a)
        ((even? n)
         (expo-iter a (square b) (/ n 2)))
        (else
          (expo-iter (* b a) b (sub1 n)))))

;; I was very close! Just wasn't thinking about it quite right


;; exercise 1.17

(define (double x)(* 2 x))
(define (halve x)(/ x 2))

(define fast-mult
  (lambda (a b)
    (cond ((eq? b 0) 0)
          ((eq? b 1) a)
          ((even? b)
           (double (fast-mult a (halve b))))
          (else
            (+ a (fast-mult a (sub1 b)))))))

;; there we go! a lovely way to multiply numbers

;; exercise 1.18

;; now we'd like to take our insights from the above and turn that into an iterative procedure

(define fast-mult
  (lambda (a b)
    (letrec ((iter-mult 
               (lambda (o e b a)
                 (cond
                   ((eq? b 0) 0)
                   ((eq? b 1)(+ o (double e)))
                   ((even? b)
                    (iter-mult o (+ e a)(halve b) a))
                   (else
                     (cond ((and (eq? o 0)(eq? e 0))
                            (iter-mult a e (sub1 b) a))
                           (else
                             (iter-mult o (+ e a)(sub1 b) a))))))))
      (iter-mult 0 0 b a))))
                      
;; above is my wacky attempt which doens't make sense or work

(define m-halve
  (lambda (n)
    (cond ((even? n)(/ n 2))
          (else
            (/ (sub1 n) 2)))))


(define multiplication-is-for-peasants
  (lambda (a b)
    (letrec ((iter-mult
               (lambda (n a b)
                 (cond ((or (eq? a 0)(eq? b 0)) 0)
                       ((eq? b 1)(+ n a))
                       ((even? b)
                        (iter-mult n (double a)(halve b)))
                       (else
                         (iter-mult (+ n a)(double a)(m-halve b)))))))
      (iter-mult 0 a b))))

;; ahh that's done it


;;exercise 1.19

(define fib
  (lambda (n)
    (letrec ((fib-iter
               (lambda (a b p q count)
                 (cond ((eq? count 0) b)
                       ((even? count)
                        (fib-iter a b
                                  (+ (* p p)(* q q))
                                  (+ (* 2 p q)␅(* q q))
                                  (/ count 2)))
                       (else (fib-iter (+ (* b q)(* a q)(* a p))
                                       (+ (* b p)(* a q))
                                       p
                                       q
                                       (- count 1)))))))
      (fib-iter 1 0 0 1 n))))


;; nifty! this is so much faster than the linear recursive one it's ridiculous
;; this algo could come in handy for some of the project euler questions!
               
;; a nice algorithm for computing the greatest common denominator of two numbers
;; is euclid's algorithm

(define euclid-gcd
  (lambda (a b)
    (if (= b 0)
      a
      (euclid-gcd b (remainder a b)))))

;; exercise 1.20
;; we'd like to evaluate (euclid-gcd 206 40) in both normal-order-evaluation
;; and applicative-order-evaluation

;; applicative order:

(euclid-gcd 206 40)
(euclid-gcd 40 (remainder 206 40))
(euclid-gcd 40 6)
(euclid-gcd 6 (remainder 40 6))
(euclid-gcd 6 4)
(euclid-gcd 4 (remainder 6 4))
(euclid-gcd 4 2)
(euclid-gcd 2 (remainder 4 2))
(euclid-gcd 2 0)
0

;; normal order looks something like

(euclid-gcd 206 40)
(euclid-gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
  40
  (euclid-gcd (remainder 206 40) 

;; and on and on and on..
;; according to the website http://eli.thegreenplace.net/2007/07/04/sicp-sections-124-125/
;; this results in 18 total calls to remainder, obviously this is going to be quite a bit less efficient.













