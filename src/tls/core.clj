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
      (list? (first list)) (cons (first (first list)) (firsts (rest list)))
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