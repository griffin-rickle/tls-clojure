(ns tls.core-test
  (:require [tls.core :refer :all])
  (:require [clojure.test :refer :all]))

(deftest test-atom
  (testing "Testing atom? call")
  (is (= true (atom? "a")))
  (is (= false (atom? ())))

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
    (is (= (list 1 2 3) (firsts (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))))
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
    (is (= 1 (length (list 1))))
    (is (= 6 (length (list "hotdogs" "with" "mustard" "sauerkraut" "and" "pickles"))))
    (is (= 5 (length (list "ham" "and" "cheese" "on" "rye"))))))

(deftest pick-test
  (testing "Testing pick function"
    (is (= "macaroni" (pick 4 (list "lasagna" "spaghetti" "ravioli" "macaroni" "meatball"))))))

(deftest rempick-test
  (testing "Testing rempick function"
    (is (= (list "hotdogs" "with" "mustard") (rempick 3 (list "hotdogs" "with" "hot" "mustard"))))))

(deftest no-nums-test
  (testing "Testing no-nums function"
    (is (= (list "pears" "prunes" "dates") (no-nums (list 5 "pears" 6 "prunes" 9 "dates"))))))

(deftest all-nums-test
  (testing "Testing all-nums function"
    (is (= (list 5 6 9) (all-nums (list 5 "pears" 6 "prunes" 9 "dates"))))))

(deftest eqan-test
  (testing "Testing eqan function"
    (is (= true (eqan? 5 5)))
    (is (= true (eqan? "asd" "asd")))
    (is (= false (eqan? 5 "asd")))))

(deftest occur-test
  (testing "Testing occur function"
    (is (= 1 (occur 5 (list 5 4 3 2 1))))
    (is (= 1 (occur "asd" (list "asd" "ASD" 5 6))))
    (is (= 5 (occur "asd" (list "asd" 1 "asd" 2 "asd" 3 "asd" 4 "asd" 5))))
    (is (= 5 (occur 1 (list 1 "asd" 1 "asdasd" 1 "asdasdasdfd" 1 "sjkgjh" 1))))))

(deftest rempick-one-test
  (testing "Testing rempick function using custom one? function"
    (is (= (list "lemon" "meringue" "pie") (rempick-one 3 (list "lemon" "meringue" "salty" "pie"))))))

(deftest rember*-test
  (testing "Testing rember* function"
    (is (= (list (list "coffee") (list (list "tea")) (list "and" (list "hick"))) (rember* "cup" (list (list "coffee") "cup" (list (list "tea") "cup") (list "and" (list "hick") "cup")) )))))

(deftest insertR*-test
  (testing "Testing insertR*"
    (is (= (list (list "how" "much" (list "wood")) "could" (list (list "a" (list "wood") "chuck" "roast")) (list (list (list "chuck" "roast"))) (list "if" (list "a") (list (list "wood" "chuck" "roast"))) "could" "chuck" "roast" "wood") (insertR* "roast" "chuck" (list (list "how" "much" (list "wood")) "could" (list (list "a" (list "wood") "chuck")) (list (list (list "chuck"))) (list "if" (list "a") (list (list "wood" "chuck"))) "could" "chuck" "wood")) ))))

(deftest occur*-test 
  (testing "Testing occur*"
    (is (= 5 (occur* "banana" (list (list "banana") (list "split" (list (list (list (list "banana" "ice"))) (list "cream" (list "banana")) "sherbet")) (list "banana") (list "bread") (list "banana" "brandy")))))))

(deftest substr*-test
  (testing "Testing subst*"
    (is (= (list (list "orange") (list "split" (list (list (list (list "orange" "ice"))) (list "cream" (list "orange")) "sherbet")) (list "orange") (list "bread") (list "orange" "brandy")) (subst* "orange" "banana" (list (list "banana") (list "split" (list (list (list (list "banana" "ice"))) (list "cream" (list "banana")) "sherbet")) (list "banana") (list "bread") (list "banana" "brandy"))) ))))

(deftest insertL*-test
  (testing "Testing insertL* function"
    (is (= (list (list "how" "much" (list "wood")) "could" (list (list "a" (list "wood") "pecker" "chuck")) (list (list (list "pecker" "chuck")) (list "if" (list "a") (list (list "wood" "pecker" "chuck"))) "could" "pecker" "chuck" "wood") (insertL* "pecker" "chuck" (list (list "how" "much" (list "wood")) "could" (list (list "a" (list "wood") "chuck")) (list (list (list "chuck"))) (list "if" (list "a") (list (list "wood" "chuck"))) "could" "chuck" "wood")))))))

(deftest member*-test
  (testing "Testing member* function"
    (is (= true (member* "chips" (list (list "potato") (list "chips" (list (list "with") "fish") (list "chips"))))))))

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

(deftest numbered-test
  (testing "Testing numbered function"
    (is (= true (numbered? (list 3 '+ 4))))
    (is (= true (numbered? (list 3 '+ (list 4 'pow 5)))))
    (is (= false (numbered? (list 2 '* "sausage"))))))

(deftest value-test
  (testing "Testing value function"
    (is (= 13 (value 13)))
    (is (= 4 (value (list 1 '+ 3))))
    (is (= 82 (value (list 1 '+ (list 3 'pow 4)))))))

; (deftest sadd-test
;   (testing "Testing sadd function"
;     (println(sadd (list) (list (list) (list))))))

(deftest is-set-test
  (testing "Testing set function"
    (is (= false (is-set (list "apple" "peaches" "apple" "plum"))))
    (is (= true (is-set (list "apples" "peaches" "pears" "plums"))))
    (is (= true (is-set (list))))))

(deftest make-set-test
  (testing "Testing make set"
    (is (= (list "apple" "peach" "pear" "plum" "lemon") (make-set (list "apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach"))))
    (is (= (list "apple" 3 "pear" 4 9) (make-set (list "apple" 3 "pear" 4 9 "apple" 3 4))))))

(deftest subset-test
  (testing "Testing subset method"
    (is (= true (subset? (list 5 "chicken" "wings") (list 5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings"))))
    (is (= false (subset? (list 4 "pounds" "of" "horseradish") (list "four" "pounds" "chicken" "and" 5 "ounces" "horseradish"))))))

(deftest eqset-test
  (testing "Testing eqset method"
    (is (= true (eqset? (list 6 "large" "chickens" "with" "wings") (list 6 "chickens" "with" "large" "wings"))))))

(deftest intersect?-test
  (testing "Testing intersect? method"
    (is (= true (intersect? (list "stewed" "tomatoes" "and" "macaroni") (list "macaroni" "and" "cheese"))))))

(deftest intersect-test
  (testing "Testing intersect method"
    (is (= (list "and" "macaroni") (intersect (list "stewed" "tomatoes" "and" "macaroni") (list "macaroni" "and" "cheese"))))))

(deftest union-test
  (testing "Testing union function"
    (is (= (list "stewed" "tomatoes" "casserole" "macaroni" "and" "cheese") (union (list "stewed" "tomatoes" "and" "macaroni" "casserole") (list "macaroni" "and" "cheese"))))))

(deftest intersectall-test
  (testing "Testing intersectall function"
    (is (= (list "a") (intersectall (list (list "a" "b" "c") (list "c" "a" "d" "e") (list "e" "f" "g" "h" "a" "b")))))
    (is (= (list 6 "and") (intersectall (list (list 6 "pears" "and") (list 3 "peaches" "and" 6 "peppers") (list 8 "pears" "and" 6 "plums") (list "and" 6 "prunes" "with" "some" "apples")))))))

(deftest a-pair-test
  (testing "Testing a-pair function"
    (is (= true (a-pair? (list "pear" "pear"))))
    (is (= true (a-pair? (list 3 7))))
    (is (= true (a-pair? (list (list 2) (list "pair")))))
    (is (= true (a-pair? (list "full" (list "house")))))))

(deftest fun-test
  (testing "Testing fun? function"
    (is (= false (fun? (list (list 4 3) (list 4 2) (list 7 6) (list 6 2) (list 3 4)))))
    (is (= true (fun? (list (list 8 3) (list 4 2) (list 7 6) (list 6 2) (list 3 4)))))
    (is (= false (fun? (list (list "d" 4) (list "b" 0) (list "b" 9) (list "e" 5) (list "g" 4)))))))

(deftest revrel-test
  (testing "Testing revrel function"
    (is (= (list (list "a" 8) (list "pie" "pumpkin") (list "sick" "got")) (revrel (list (list 8 "a") (list "pumpkin" "pie") (list "got" "sick")))))))

(deftest fullfun-test
  (testing "Testing fullfun? function"
    (is (= false (fullfun? (list (list 8 3) (list 4 2) (list 7 6) (list 6 2) (list 3 4)))))
    (is (= false (fullfun? (list (list "grape" "raisin") (list "plum" "prune") (list "stewed" "prune")))))
    (is (= true (fullfun? (list (list 8 3) (list 4 8) (list 7 6) (list 6 2) (list 3 4)))))))

(deftest rember-f-test
  (testing "Testing rember-f function"
    (is (= (list "salad" "is" "good") ((rember-f equal?) "tuna" (list "tuna" "salad" "is" "good"))))
    (is (= (list "shrimp" "salad" "and" "salad") ((rember-f equal?) "tuna" (list "shrimp" "salad" "and" "tuna" "salad"))))))

(deftest insertL-g-test
  (testing "Testing insertL function created from passing seqL to insert-g"
    (is (= (list "ice" "cream" "with" "topping" "fudge" "for" "dessert") (insertL-g "topping" "fudge" (list "ice" "cream" "with" "fudge" "for" "dessert"))))
    (is (= (list "tacos" "tamales" "jalapeno" "and" "salsa" ) (insertL-g "jalapeno" "and" (list "tacos" "tamales" "and" "salsa"))))
    (is (= (list "a" "b" "c" "e" "d" "f" "g" "d" "h") (insertL-g "e" "d" (list "a" "b" "c" "d" "f" "g" "d" "h"))))))