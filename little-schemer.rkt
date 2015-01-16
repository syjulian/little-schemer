#lang racket
;; the little schemer

;; CHAPTER 1

(define atom?
;; checks if x is an atom
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; CHAPTER 2

(define lat?
;; checks if x is list of atoms
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define member?
;; checks if member a is in list lat
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

;; CHAPTER 3

(define rember
;; removes first member from list lat equal to a
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define firsts
;; lists first members of lists in list l
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) 
             (firsts (cdr l)))))))

(define seconds
;; lists second members of lists in list l
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (cdr (car l))) 
             (seconds (cdr l)))))))

(define insertR
;; finds first member in list lat equal to old and adds new to the right 
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons old
                     (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))

(define insertL
;; finds first member in list lat equal to old and adds new to the left
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons new lat))
               (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

(define subst
;; substitutes first member in list lat equal to old with new
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons new (cdr lat)))
               (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

(define subst2
;; substitutes first member in list lat equal to either o1 or o2 with new      
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((or (eq? o1 (car lat)) (eq? o2 (car lat))) 
               (cons new (cdr lat)))
               (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

(define multirember
;; removes all members in list lat equal to a
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
;; finds all members in list lat equal to old and adds new to the right
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons old
                     (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))))

(define multiinsertL
;; finds all members in list lat equal to old and adds new to the left
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons new 
                     (cons old (multiinsertL new old (cdr lat)))))
               (else (cons (car lat)
                          (multiinsertL new old (cdr lat)))))))))

(define multisubst
;; substitutes all members in list lat equal to old with new
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? old (car lat)) 
               (cons new (multisubst new old (cdr lat))))
               (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))

;; CHAPTER 4

(define add1
;; adds 1 to n
  (lambda (n)
    (+ n 1)))

(define sub1
;; subtracts 1 from n 
  (lambda (n)
    (- n 1)))

(define o+
;; adds m to n
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
;; subtracts m from n
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define addtup
;; sums up tuple
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
;; multiplies n by m
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

(define tup+
;; adds numbers of same index in tup1 to tup2
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1) 
      (else 
       (cons (o+ (car tup1) (car tup2)) 
                  (tup+ 
                   (cdr tup1) (cdr tup2)))))))

(define o>
;; checks if n is greater than m
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
;; checks if n is less than m
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
;; checks if n is equal to m
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(define oexpt
;; raises n to the mth power
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (oexpt n (sub1 m)))))))

(define oquotient
;; divides n by m
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (oquotient (o- n m) m))))))

(define length
;; counts number of member in lat
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
;; returns nth member in lat
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
;; removes nth member in lat
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) 
                  (rempick (sub1 n) 
                           (cdr lat)))))))

(define no-nums
;; removes numbers from lat
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat)) 
               (no-nums (cdr lat)))
              (else (cons (car lat) 
                          (no-nums 
                           (cdr lat)))))))))

(define all-nums
;; returns tuple of numbers from lat
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else 
       (cond
         ((number? (car lat))
          (cons (car lat)
                (all-nums (cdr lat))))
         (else (all-nums (cdr lat))))))))

(define eqan?
;; checks if a1 and a2 are equal
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(define occur
;; returns number of occurrences of a in lat
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? a (car lat)) 
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

(define one?
;; checks if n is equal to 1
  (lambda (n)
    (= n 1)))

;; CHAPTER 5

(define rember*
;; removes a from list l and its list members
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((not (atom? (car l))) 
       (cons (rember* a (car l)) (rember* a (cdr l))))
      (else (cond
              ((eq? a (car l)) (rember* a (cdr l)))
              (else (cons (car l) (rember* a (cdr l)))))))))

(define insertR*
;; inserts new after instances of old in list l and its list members
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old
                (cons new 
                      (insertR* new old 
                                (cdr l)))))
         (else (cons (car l) 
                     (insertR* new old 
                               (cdr l))))))
      (else (cons (insertR* new old 
                            (car l)) 
                  (insertR* new old 
                            (cdr l)))))))

(define occur*
;; counts occurrences of a in list l and its list members
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
                (occur* a (cdr l)))))))
    
(define subst*
;; substitutes occurences of a in list l and its list members
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                (subst* new old (cdr l)))))))

(define insertL*
;; inserts new before instances of old in list l and its list members
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old 
                      (insertL* new old 
                                (cdr l)))))
         (else (cons (car l) 
                     (insertL* new old 
                               (cdr l))))))
      (else (cons (insertL* new old 
                            (car l)) 
                  (insertL* new old 
                            (cdr l)))))))

(define member*
;; checks if a is in list l or its list members
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define leftmost
;; finds left most atom in a list of lists and atoms
  (lambda (l)
    (cond
      ((atom? (car l) (car l)))
      (else (leftmost (car l))))))

(define equal
;; checks if s expression s1 and s2 are equal
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
;; checks if lists l1 and l2 are equal
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(define rember-refined
;; checks if s-expression s is in l
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))

;; CHAPTER 6

(define numbered?
;; checks if aexp is an infix notation arithmetic expression
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define value
;; evaluates prefix notation arithmetic expression nexp
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote *))
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else
       (expt (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp
;; returns 1st sub-expression of aexp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
;; returns 2nd sub-expression of aexp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
;; returns operator of aexp
  (lambda (aexp)
    (car aexp)))

(define sero?
;; checks if zero in list form
  (lambda (n)
    (null? n)))

(define edd1
;; adds one in list form
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
;; subs one in list form
  (lambda (n)
    (cdr n)))

(define +list
;; adds numbers in list form
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (+list n (zub1 m)))))))

;; CHAPTER 7

(define set?
;; checks if lat is a set
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
;; creates set out of lat
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
                  (makeset 
                   (multirember 
                    (car lat) (cdr lat))))))))

(define subset?
;; checks if set1 is a subset of set2
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
       (subset? (cdr set1) set2))))))

(define eqset?
;; checks if set1 and set2 are equal
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
;; checks if set1 and set2 intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(define intersect
;; gets intersection of set1 and set2
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1) 
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
;; gets union of set1 and set2
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
             (union (cdr set1) set2))
      (else (cons (car set1) 
                  (union 
                   (cdr set1) set2))))))

(define difference
;; gets difference of set1 and set2
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
             (difference (cdr set1) set2))
      (else (cons (car set1) 
                  (difference 
                   (cdr set1) set2))))))

(define intersectall
;; gets intersection of all sets in l-set
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(define a-pair?
;; checks if x is a pair
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
;; returns first s-expression in p
  (lambda (p)
    (car p)))

(define second
;; returns second s-expression in p
  (lambda (p)
    (car (cdr p))))

(define build
;; builds a pair from s1 and s2
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define third
;; returns third s-expression in l
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
;; checks whether rel is a function
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
;; reverses pairs in rel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(define revpair
;; reverses pair pair
  (lambda (pair)
    (cond 
      (build (second pair) (first pair)))))

(define fullfun?
;; checks if fun is a full function
 (lambda (fun)
   (cond
     (set? (seconds fun)))))

(define one-to-one?
;; checks if is one to one / the same as fullfun
  (lambda (fun)
    (cond
      (fun? (revrel fun)))))

;; CHAPTER 8

(define rember-f-old
;; removes element that returns true on (test? element a) in l
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l) 
                  (rember-f-old test? a 
                            (cdr l)))))))

(define eq?-c
;; returns a function that checks if x is equal to a
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
;; checks if function is equal to salad
  (eq?-c 'salad))

(define rember-f
;; returns a function that rembers an element where test? is true
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l) 
                    ((rember-f test?) a
                                      (cdr l))))))))      

(define insertL-f
;; returns a function that inserts to the new before old of an element where test? is true
  (lambda (test?)
    (lambda (old new l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old
                                       (cdr l))))))))
  
(define insertR-f
;; returns a function that inserts to the new after old of an element where test? is true
  (lambda (test?)
    (lambda (old new l)
      (cond
        ((null? l) (quote ()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old
                                       (cdr l))))))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) 
                     new old (cdr l))))))))

(define insert-L
  (insert-g seqL))

(define insert-R
  (insert-g seqR))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-new
  (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember-new
  (insert-g seqrem))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? (quote +) x) +)
      ((eq? (quote *) x) *)
      (else expt))))

(define value-new
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value-new (1st-sub-exp nexp))
             (value-new (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat)) 
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) 
                     a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))
        
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat)) 
       ((multiremberT test? (cdr lat))))
      (else (cons (car lat)
                  ((multiremberT test?) 
                   (cdr lat)))))))

(define multirember&co
;; applies col to list of elem in lat equal to a with the rest of lat
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote ()) (quote ())))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat ) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? oldL (car lat))
       (cons new
             (cons oldL 
                       (multiinsertLR new oldL oldR 
                                      (cdr lat)))))
      ((eq? oldR (car lat))
       (cons oldR
             (cons new 
                       (multiinsertLR new oldL oldR 
                                      (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR 
                                      (cdr lat)))))))

(define multiinsertLR&co
;; does multiinsertLR to lat then uses col on final list and number of oldL's and oldR's in lat
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) 
       (col (quote ()) 0 0))
      ((eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)   
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R))))
      ((eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)   
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR
                         (cdr lat)
                         (lambda (newlat L R)   
                           (col (cons (car lat) newlat)
                                L R)))))))


(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(define evens-only*&co
;; returns list and product of evens and sum of odds
  (lambda (l col)
    (cond
      ((null? l) 
       (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* (car l) p) s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p (+ (car l) s)))))))
       (else
        (evens-only*&co (car l)
                        (lambda (al ap as)
                          (evens-only*&co (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))
 
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
;;shifts first pair's second into second pair
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
;; total
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))))))

(define shuffle
;; partial  
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define C
;; collatz conjecture
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (add1 (* 3 n)))))))

(define A
;; ackermann function
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(define eternity
;; never ending recursion - partial
  (lambda (x)
    (eternity x)))

;; length function
((lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(define Y
;; applicative-order Y combinator
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; CHAPTER 10

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          (entry-f))))

(define new-entry build)

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name)
       (car values))
      (else (lookup-in-entry-help 
             name
             (cdr names)
             (cdr values)
             entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry name 
                        (car table) 
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           (table-f))))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))
      
(define val
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))
      
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
            (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda(l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda(l)
    (eq? (first l) (quote non-primitive))))

(define applyfun
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive
        (second fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))       
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))       
      ((eq? name (quote zero?))
       (zero? (first vals)))       
      ((eq? name (quote add1))
       (add1 (first vals)))       
      ((eq? name (quote sub1))
       (sub1 (first vals)))       
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))
       