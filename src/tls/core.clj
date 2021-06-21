(ns tls.core)

(defn atom? [x] (not (list? x)))

(defn lat? [list]
  (if (empty? list)
    true
    (if (atom? (first list))
      (recur (rest list))
      false)))

(defn member?
  [x list]
    (cond
      (empty? list) false
      (= x (first list)) true
      :else (member? x (rest list))))

(defn rember
  [x list]
    (cond
      (empty? list) ()
      (= x (first list)) (rest list)
      :else (cons (first list) (rember x (rest list)))))

(defn firsts
  [list]
    (cond
      (empty? list) ()
      (or (seq? list) (list? (first list))) (cons (first (first list)) (firsts (rest list)))
      :else ()))

(defn insertL
  [new old list] 
    (cond
      (empty? list) ()
      (= old (first list)) (cons new list)
      :else (cons (first list) (insertL new old (rest list)))))

(defn insertR
  [new old list]
    (cond
      (empty? list) ()
      (= (first list) old) (cons old (cons new (rest list)))
      :else (cons (first list) (insertR new old (rest list)))))

(defn subst 
  [new old list]
    (cond
      (empty? list) ()
      (= old (first list)) (cons new (rest list))
      :else (cons (first list) (subst new old (rest list)))))

(defn subst2
  [new o1 o2 list]
    (cond
      (empty? list) ()
      (or (= o1 (first list)) (= o2 (first list))) (cons new (rest list))
      :else (cons (first list) (subst2 new o1 o2 (rest list)))))

(defn multirember
  [x list]
    (cond
      (empty? list) ()
      (= x (first list)) (multirember x (rest list))
      :else (cons (first list) (multirember x (rest list)))))

(defn multiinsertR
  [new old list]
    (cond
      (empty? list) ()
      (= old (first list)) (cons old (cons new (multiinsertR new old (rest list))))
      :else (cons (first list) (multiinsertR new old (rest list)))))

(defn multiinsertL
  [new old list]
    (cond
      (empty? list) ()
      (= old (first list)) (cons new (cons old (multiinsertL new old (rest list))))
      :else (cons (first list) (multiinsertL new old (rest list)))))

(defn multisubst
  [new old list]
    (cond
      (empty? list) ()
      (= old (first list)) (cons new (multisubst new old (rest list)))
      :else (cons (first list) (multisubst new old (rest list)))))

(defn add
  [n m]
    (cond
      (zero? m) n
      :else (add (+ n 1) (- m 1))))

(defn sub
  [n m]
    (cond
      (zero? m) n
      :else (sub (- n 1) (- m 1))))

(defn addtup
  [tup]
    ((cond
      (empty? tup) 0
      :else (+ (first tup) (addtup (rest tup))))))

(defn mult
  [m n]
    (cond
      (zero? m) 0
      :else (+ n (mult (- m 1) n))))

(defn tupAdd
  [tup1 tup2]
    (cond
      (and (empty? tup1) (empty? tup2)) ()
      (empty? tup1) tup2
      (empty? tup2) tup1
      :else (cons (+ (first tup1) (first tup2)) (tupAdd (rest tup1) (rest tup2)))))

(defn gt
  [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (gt (- n 1) (- m 1))))
    
(defn lt
  [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (lt (- n 1) (- m 1))))

(defn eq 
  [n m]
  (cond
    (or (lt n m) (gt n m)) false
    :else true))

(defn pow
  [n m] 
  (cond
    (= m 0) 1
    :else (mult n (pow n (- m 1)))))

(defn div
  [n m]
  (cond
    (lt n m) 0
    :else (+ 1 (div (sub n m) m))))

(defn length
  [list]
  (cond
    (empty? list) 0
    :else (+ 1 (length (rest list)))))

(defn pick
  [n list]
  (cond
    (eq n 1) (first list)
    :else (pick (- n 1) (rest list))))

(defn rempick
  [n list]
  (cond
    (eq 1 n) (rest list)
    :else (cons (first list) (rempick (- n 1) (rest list)))))

(defn no-nums
  [list]
  (cond
    (empty? list) ()
    (number? (first list)) (no-nums (rest list))
    :else (cons (first list) (no-nums (rest list)))))

(defn all-nums
  [list]
  (cond
    (empty? list) ()
    (number? (first list)) (cons (first list) (all-nums (rest list)))
    :else (all-nums (rest list))))

(defn eqan?
  [a1 a2]
  (cond
    (and (atom? a1) (atom? a2)) 
      (cond
        (and (number? a1) (number? a2)) (= a1 a2)
        (or (number? a1) (number? a2)) false
        :else (= a1 a2))
    :else false))

(defn occur
  [a list]
  (cond
    (empty? list) 0
    (eqan? a (first list)) (+ 1 (occur a (rest list)))
    :else (occur a (rest list))))

(defn one?
  [n] (= 1 n))

(defn rempick-one
  [n list]
  (cond
    (one? n) (rest list)
    :else (cons (first list) (rempick-one (- n 1) (rest list)))))

(defn rember*
  [a list]
  (cond
    (empty? list) ()
    (atom? (first list))
      (cond
        (eqan? a (first list)) (rember* a (rest list))
        :else (cons (first list) (rember* a (rest list))))
    :else (cons (rember* a (first list)) (rember* a (rest list)))))

(defn insertR*
  [new old list]
  (cond
    (empty? list) ()
    (atom? (first list))
      (cond
        (eqan? (first list) old) (cons old (cons new (insertR* new old (rest list))))
        :else (cons (first list) (insertR* new old (rest list))))
    :else (cons (insertR* new old (first list)) (insertR* new old (rest list)))))

(defn occur*
  [a list]
  (cond
    (empty? list) 0
    (atom? (first list)) 
      (cond 
        (eqan? (first list) a) (+ 1 (occur* a (rest list)))
        :else (occur* a (rest list)))
    :else (+ (occur* a (first list)) (occur* a (rest list)))))

(defn subst* 
  [new old list]
  (cond
    (empty? list) ()
    (atom? (first list))
      (cond
        (eqan? (first list) old) (cons new (subst* new old (rest list)))
        :else (cons (first list) (subst* new old (rest list))))
    :else (cons (subst* new old (first list)) (subst* new old (rest list)))))

(defn insertL* 
  [new old list]
  (cond
    (empty? list) ()
    (atom? (first list))
      (cond
        (eqan? old (first list)) (cons new (cons old (insertL* new old (rest list))))
        :else (cons (first list) (insertL* new old (rest list))))
    :else (cons (insertL* new old (first list)) (insertL* new old (rest list)))))

(defn member* 
  [a list]
  (cond
    (empty? list) false
    (atom? (first list)) (or (eqan? (first list) a) (member* a (rest list)))
    :else (or (member* a (first list)) (member* a (rest list)))))

(defn leftmost
  [list]
  (cond
    (atom? (first list)) (first list)
    :else (leftmost (first list))))

(defn eqlist?
  [l1 l2]
  (cond
    (empty? l1) (empty? l2)
    (empty? l2) false
    (atom? (first l1)) (and (eqan? (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))
    (and (list? (first l1)) (list? (first l2))) (and (eqlist? (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))
    :else false))

(defn equal?
  [a b]
  (cond
    (and (atom? a) (atom? b)) (eqan? a b)
    (or (atom? a) (atom? b)) false
    :else (eqlist? a b)))

(defn eqlist-eq?
  [l1 l2]
  (cond
    (empty? l1) (empty? l2)
    (empty? l2) false
    :else (and (equal? (first l1) (first l2)) (eqlist-eq? (rest l1) (rest l2)))))

(defn numbered?
  [aexp]
  (cond
    (atom? aexp) (number? aexp)
    :else (and (numbered? (first aexp)) (numbered? (first (rest (rest aexp)))))))

(defn value
  [nexp]
  (cond
  (atom? nexp) nexp
  (= '+ (first (rest nexp))) (+ (first nexp) (value (first (rest (rest nexp)))))
  (= '* (first (rest nexp))) (* (first next) (value (first (rest (rest nexp)))))
  (= 'pow (first (rest nexp))) (pow (first nexp) (value (first (rest (rest nexp)))))))

(defn first-sub-expression
  [aexp] (first aexp))

(defn second-sub-expression
  [aexp] (first (rest (rest aexp))))

(defn operator
  [aexp] (first (rest aexp)))

(defn sero?
  [n] (empty? n))

(defn sadd1
  [n] (cons () n))

(defn ssub1
  [n] (rest n))

(defn sadd
  [n m]
  (cond
    (sero? m) (sadd1 n)
    :else (sadd (sadd1 n) (ssub1 m))))

(defn is-set
  [set]
  (cond
    (empty? set) true
    (member? (first set) (rest set)) false
    :else (is-set (rest set))))

(defn make-set
  [list]
  (cond
    (empty? list) ()
    (member? (first list) (rest list)) (make-set (cons (first list) (multirember (first list) (rest list))))
    :else (cons (first list) (make-set (rest list)))))

(defn subset?
  [set1 set2]
  (cond
    (empty? set1) true
    :else (and (member? (first set1) set2) (subset? (rest set1) set2))))

(defn eqset?
  [set1 set2] (and (subset? set1 set2) (subset? set2 set1)))

(defn intersect?
  [set1 set2] 
  (cond
    (empty? set1) false
    :else (or (member? (first set1) set2) (intersect? (rest set1) set2))))

(defn intersect
  [set1 set2]
  (cond
    (empty? set1) ()
    (member? (first set1) set2) (cons (first set1) (intersect (rest set1) set2))
    :else (intersect (rest set1) set2)))

(defn union
  [set1 set2]
  (cond
    (empty? set1) set2
    (member? (first set1) set2) (union (rest set1) set2)
    :else (cons (first set1) (union (rest set1) set2))))

(defn intersectall
  [set]
  (cond
    (empty? (rest set)) (first set)
    :else (intersect (first set) (intersectall (rest set)))))

(defn a-pair?
  [x]
  (cond
    (atom? x) false
    (empty? x) false
    (empty? (rest x)) false
    (empty? (rest (rest x))) true
    :else false))

; (defn first
;   [p] (first p))

; (defn second
;   [p] (first (rest p)))

(defn build
  [s1 s2] (cons s1 (cons s2 ())))

(defn third
  [p] (first (rest (rest p))))

(defn fun?
  [rel] (is-set (firsts rel)))

(defn revpair
  [pair] (build (second pair) (first pair)))

(defn revrel
  [rel]
  (cond
    (empty? rel) ()
    :else (cons (revpair (first rel)) (revrel (rest rel)))))

(defn seconds
  [fun] (firsts (revrel fun)))

(defn fullfun?
  [fun] (fun? (revrel fun)))

(defn rember-f
  [test?]
  (fn
    [a l]
    (cond
      (empty? l) ()
      (test? a (first l)) (rest l)
      :else (cons (first l) ((rember-f test?) a (rest l))))))

(defn insertL-f
  [test?]
  (fn
    [old new l]
    (cond
      (empty? l) ()
      (test? old (first l)) (cons new (cons old) (rest l))
      :else (cons (first l) ((insertL-f test?) old new (rest l))))))

(defn insertR-f
  [test?]
  (fn
    [old new l]
    (cond
      (empty? l) ()
      (test? old (first l)) (cons old (cons new (rest l)))
      :else (cons (first l) ((insertR-f test?) old new (rest l))))))

(defn seqL
  [new old l] (cons new (cons old l)))

(defn seqR
  [new old l] (cons old (cons new l)))

(defn insert-g
  [seq-g]
  (fn
    [new old l]
    (cond
      (empty? l) ()
      (equal? old (first l)) (seq-g new old (rest l))
      :else (cons (first l) ((insert-g seq-g) new old (rest l))))))

; (defn insertL-g [new old l] ((insert-g (fn [new old l] (cons new (cons old l)))) new old l))
(defn insertL-g [new old l] ((insert-g #(cons %1 (cons %2 %3))) new old l))

(defn insertR-g [new old l] ((insert-g #(cons %2 (cons %1 %3))) new old l))

(defn seqS [new old l] (cons new l))

(defn subst-g [new old l] ((insert-g seqS) new old l))

(defn seqrem [new old l] l)

(defn rember-g [a l] ((insert-g seqrem) nil a l))

(defn atom-to-function [x]
  (cond
    (= x '+) +
    (= x '*) *
    :else pow))

(defn value-f
  [nexp]
    (cond
      (atom? nexp) nexp
      :else ((atom-to-function (operator nexp)) (value-f (first-sub-expression nexp)) (value-f (second-sub-expression nexp)))))

(defn multirember-f
  [test?]
    (fn
      [a lat]
      (cond
        (empty? lat) ()
        (test? a (first lat)) ((multirember-f test?) a (rest lat))
        :else (cons (first lat) ((multirember-f test?) a (rest lat))))))

; (defn multirember-eq (multirember-f =))

(defn multirember-and-co 
  [a lat col]
  (cond
    (empty? lat) (col () ())
    (= a (first lat)) (multirember-and-co a (rest lat) (fn [newlat seen] (col newlat (cons (first lat) seen))))
    :else (multirember-and-co a (rest lat) (fn [newlat seen] (col (cons (first lat) newlat) seen)))))


(defn multiinsertLR
  [new oldL oldR lat]
  (cond
    (empty? lat) ()
    (= (first lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (rest lat))))
    (= (first lat) oldL) (cons new (cons new (multiinsertLR new oldL oldR (rest lat))))
    :else (cons (first lat (multiinsertLR new oldR oldL (rest lat))))))


(defn multiinsertLR-and-co
  [new oldL oldR lat col]
  (cond
    (empty? lat) (col () 0 0)
    (= oldL (first lat)) (multiinsertLR-and-co new oldL oldR (rest lat) (fn [newlat seenL seenR] (col (cons new (cons oldL newlat)) (+ 1 seenL) seenR)))
    (= oldR (first lat)) (multiinsertLR-and-co new oldL oldR (rest lat (fn [newlat seenL seenR] (col (cons oldR (cons new newlat)) seenL (+ 1 seenR)))))
    :else (multiinsertLR-and-co new oldL oldR (rest lat) (fn [newlat seenL seenR] (col (cons (first lat) newlat) seenL seenR)))))

; wrong in clojure.
; (defn is-even? [n] (= (* (/ n 2) 2) n))


(defn evens-only* 
  [list]
  (cond
    (empty? list) ()
    (and (atom? (first list)) (even? (first list))) (cons (first list) (evens-only* (rest list)))
    (atom? (first list)) (evens-only* (rest list))
    :else (cons (evens-only* (first list)) (evens-only* (rest list)))))






