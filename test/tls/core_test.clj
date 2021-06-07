(ns tls.core-test
  (:require [tls.core :refer :all])
  (:require [clojure.test :refer :all]))

(deftest test-atom
  (testing "Testing atom? call")
  (is (= true (atom? "a")))
  (is (= false (atom? ()))))

(deftest lat-test
  (testing "List of atoms"
    (is (= true (lat? (list "a"))))
    (is (= false (lat? (list (list ())))))
    (is (= true (lat? (list "a" "b" "c"))))
    (is (= false (lat? (list "a" (list "a")))))))

(deftest rember-test
  (testing "Testing rember"
    (is (= (list "a" "b" "c") (rember "d" (list "a" "b" "c" "d"))))
    (is (= (list) (rember "d" (list "d"))))
    (is (= (list "a" "b" "c" "d") (rember "e" (list "a" "b" "c" "d"))))))

(deftest test-member
  (testing "Testing member function"
  	(is (= true (member? "a" (list "a"))))
  	(is (= false (member? "a" (list "b"))))
  	(is (= true (member? "a" (list "s" "d" "v" "a"))))))

(deftest firsts-test
  (testing "Testing firsts functionality"
    (is (= (list "a" "b" "c") (firsts (list (list "a") (list "b") (list "c")))))
    (is (= (list) (firsts (list))))))

(deftest insertR-test
  (testing "Testing insertR"
    (is (= (list "ice" "cream" "with" "fudge" "topping" "for" "dessert") (insertR "topping" "fudge" (list "ice" "cream" "with" "fudge" "for" "dessert"))))
    (is (= (list "tacos" "tamales" "and" "jalapeno" "salsa") (insertR "jalapeno" "and" (list "tacos" "tamales" "and" "salsa"))))
    (is (= (list "a" "b" "c" "d" "e" "f" "g" "d" "h") (insertR "e" "d" (list "a" "b" "c" "d" "f" "g" "d" "h"))))))

(deftest insertL-test
  (testing "Testing insertL"
    (is (= (list "ice" "cream" "with" "topping" "fudge" "for" "dessert") (insertL "topping" "fudge" (list "ice" "cream" "with" "fudge" "for" "dessert"))))
    (is (= (list "tacos" "tamales" "jalapeno" "and" "salsa" ) (insertL "jalapeno" "and" (list "tacos" "tamales" "and" "salsa"))))
    (is (= (list "a" "b" "c" "e" "d" "f" "g" "d" "h") (insertL "e" "d" (list "a" "b" "c" "d" "f" "g" "d" "h"))))))

(deftest subst-test
  (testing "Testing subst method"
    (is (= (list "ice" "cream" "with" "topping" "for" "dessert") (subst "topping" "fudge" (list "ice" "cream" "with" "fudge" "for" "dessert"))))
    (is (= (list "tacos" "tamales" "jalapeno" "salsa" ) (subst "jalapeno" "and" (list "tacos" "tamales" "and" "salsa"))))
    (is (= (list "a" "b" "c" "e" "f" "g" "d" "h") (subst "e" "d" (list "a" "b" "c" "d" "f" "g" "d" "h"))))))

(deftest subst2-test
  (testing "Testing subst2"
    (is (= (list "vanilla" "ice" "cream" "with" "chocolate" "topping") (subst2 "vanilla" "chocolate" "banana" (list "banana" "ice" "cream" "with" "chocolate" "topping"))))))

(deftest multirember-test
  (testing "Testing Multirember function"
    (is (= (list "coffee" "tea" "and" "hick") (multirember "cup" (list "coffee" "cup" "tea" "cup" "and" "hick" "cup"))))))

(deftest multiinsertL-test
  (testing "Testing multiinsertL function"
    (is (= (list "chips" "and" "fried" "fish" "or" "fried" "fish" "and" "fried") (multiinsertL "fried" "fish" (list "chips" "and" "fish" "or" "fish" "and" "fried"))))))

(deftest multisubst-test
  (testing "Testing multisubst"
    (is (= (list "a" "a" "b" "b") (multisubst "b" "c" (list "a" "a" "c" "c"))))))

(deftest add-test
	(testing "Testing add function"
		 (is (= 58 (add 46 12)))))

(deftest tupAdd-test
  (testing "Testing Tuple Add"
    (is (= (list 11 11 11 11 11) (tupAdd (list 3 6 9 11 4) (list 8 5 2 0 7))))
    (is (= (list 7 13) (tupAdd (list 3 7) (list 4 6))))))

(deftest sub-test
  (testing "Testing subtraction function"
    (is (= 11 (sub 14 3)))
    (is (= 8 (sub 17 9)))))

(deftest mult-test
  (testing "Testing multiplication"
    (is (= 8 (mult 4 2)))
    (is (= 15 (mult 5 3)))
    (is (= 36 (mult 12 3)))))

(deftest gt-test
  (testing "Testing Greater than function"
    (is (= false (gt 12 133)))
    (is (= true (gt 120 11)))))

(deftest lt-test
  (testing "Testing less than function"
    (is (= true (lt 4 5)))
    (is (= false (lt 5 5)))))

(deftest eq-test
  (testing "Testing equality function"
    (is (= true (eq 1 1)))
    (is (= false (eq 2 1)))))

(deftest pow-test
  (testing "Testing power function"
    (is (= 1 (pow 1 1)))
    (is (= 8 (pow 2 3)))
    (is (= 125 (pow 5 3)))))

(deftest div-test
  (testing "Testing Division function"
    (is (= 3 (div 15 4)))))

(deftest length-test
  (testing "Testing length function"
    (is (= 1 (length '(1))))
    (is (= 6 (length '("hotdogs" "with" "mustard" "sauerkraut" "and" "pickles"))))
    (is (= 5 (length '("ham" "and" "cheese" "on" "rye"))))))

(deftest pick-test
  (testing "Testing pick function"
    (is (= "macaroni" (pick 4 '("lasagna" "spaghetti" "ravioli" "macaroni" "meatball"))))))

(deftest rempick-test
  (testing "Testing rempick function"
    (is (= '("hotdogs" "with" "mustard") (rempick 3 '("hotdogs" "with" "hot" "mustard"))))))

(deftest no-nums-test
  (testing "Testing no-nums function"
    (is (= '("pears" "prunes" "dates") (no-nums '(5 "pears" 6 "prunes" 9 "dates"))))))

(deftest all-nums-test
  (testing "Testing all-nums function"
    (is (= '(5 6 9) (all-nums '(5 "pears" 6 "prunes" 9 "dates"))))))

(deftest eqan-test
  (testing "Testing eqan function"
    (is (= true (eqan? 5 5)))
    (is (= true (eqan? "asd" "asd")))
    (is (= false (eqan? 5 "asd")))))

(deftest occur-test
  (testing "Testing occur function"
    (is (= 1 (occur 5 '(5 4 3 2 1))))
    (is (= 1 (occur "asd" '("asd" "ASD" 5 6))))
    (is (= 5 (occur "asd" '("asd" 1 "asd" 2 "asd" 3 "asd" 4 "asd" 5))))
    (is (= 5 (occur 1 '(1 "asd" 1 "asdasd" 1 "asdasdasdfd" 1 "sjkgjh" 1))))))

(deftest rempick-one-test
  (testing "Testing rempick function using custom one? function"
    (is (= '("lemon" "meringue" "pie") (rempick-one 3 '("lemon" "meringue" "salty" "pie"))))))

(deftest rember*-test
  (testing "Testing rember* function"
    (is (= '('("coffee") '('("tea")) '("and" '("hick"))) (rember* "cup" '('("coffee") "cup" '('("tea") "cup") '("and" '("hick") "cup")) )))))

(deftest insertR*-test
  (testing "Testing insertR*"
    (is (= '('("how" "much" '("wood")) "could" '('("a" '("wood") "chuck" "roast")) '('('("chuck" "roast"))) '("if" '("a") '('("wood" "chuck" "roast"))) "could" "chuck" "roast" "wood") (insertR* "roast" "chuck" '('("how" "much" '("wood")) "could" '('("a" '("wood") "chuck")) '('('("chuck"))) '("if" '("a") '('("wood" "chuck"))) "could" "chuck" "wood")) ))))

(deftest occur*-test 
  (testing "Testing occur*"
    (is (= 5 (occur* "banana" '('("banana") '("split" '('('('("banana" "ice"))) '("cream" '("banana")) "sherbet")) '("banana") '("bread") '("banana" "brandy")))))))

(deftest substr*-test
  (testing "Testing subst*"
    (is (= '('("orange") '("split" '('('('("orange" "ice"))) '("cream" '("orange")) "sherbet")) '("orange") '("bread") '("orange" "brandy")) (subst* "orange" "banana" '('("banana") '("split" '('('('("banana" "ice"))) '("cream" '("banana")) "sherbet")) '("banana") '("bread") '("banana" "brandy"))) ))))

(deftest insertL*-test
  (testing "Testing insertL* function"
    (is (= '('("how" "much" '("wood")) "could" '('("a" '("wood") "pecker" "chuck")) '('('("pecker" "chuck")) '("if" '("a") '('("wood" "pecker" "chuck"))) "could" "pecker" "chuck" "wood") (insertL* "pecker" "chuck" '('("how" "much" '("wood")) "could" '('("a" '("wood") "chuck")) '('('("chuck"))) '("if" '("a") '('("wood" "chuck"))) "could" "chuck" "wood")))))))

(deftest member*-test
  (testing "Testing member* function"
    (is (= true (member* "chips" '('("potato") '("chips" '('("with") "fish") '("chips"))))))))

(deftest leftmost-test
  (testing "Testing leftmost function"
    (is (= "potato" (leftmost (list ( list "potato" ) (list "chips" (list (list "with") "fish") (list "chips"))))))
    (is (= "hot" (leftmost (list (list (list "hot") (list "tuna" (list "and"))) "cheese"))))))

(deftest eqlist-test
  (testing "Testing eqlist function"
    (is (= true (eqlist? (list "strawberry" "ice" "cream") (list "strawberry" "ice" "cream"))))
    (is (= false (eqlist? (list "strawberry" "ice" "cream") (list "strawberry" "cream" "ice"))))
    (is (= false (eqlist? (list "banana" (list (list "split"))) (list (list "banana") (list "split")))))
    (is (= false (eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda"))) (list "beef" (list (list "salami")) (list "and" (list "soda"))))))
    (is (= true (eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda"))) (list "beef" (list (list "sausage")) (list "and" (list "soda"))))))))

(deftest eqlist-eq-test
  (testing "Testing eqlist function"
    (is (= true (eqlist-eq? (list "strawberry" "ice" "cream") (list "strawberry" "ice" "cream"))))
    (is (= false (eqlist-eq? (list "strawberry" "ice" "cream") (list "strawberry" "cream" "ice"))))
    (is (= false (eqlist-eq? (list "banana" (list (list "split"))) (list (list "banana") (list "split")))))
    (is (= false (eqlist-eq? (list "beef" (list (list "sausage")) (list "and" (list "soda"))) (list "beef" (list (list "salami")) (list "and" (list "soda"))))))
    (is (= true (eqlist-eq? (list "beef" (list (list "sausage")) (list "and" (list "soda"))) (list "beef" (list (list "sausage")) (list "and" (list "soda"))))))))
