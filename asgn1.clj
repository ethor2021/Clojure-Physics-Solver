(load-file "cs378.clj")

; 2a - sum in recursive style

(defn sum [lst]
	(if (empty? lst) 
	    0 
	    (+ (first lst) (sum (rest lst)))))

; 2b - sumtr in tail recursive style

(defn sumtrb [lst ans]
	(if (empty? lst)
	ans
	(sumtrb (rest lst) (+ ans (first lst)))))

(defn sumtr [lst]
	(sumtrb lst 0))

; 2c - sumr using reduce

(defn sumr [lst]
	(reduce + lst))

; 3a - sumsq in recursive style

(defn sumsq [lst]
	(if (empty? lst)
	0
	(+ (Math/pow (first lst) 2) (sumsq (rest lst)))))

; 3b - sumsqtr in tail recrusive style

(defn sumsqtrb [lst ans]
	(if (empty? lst)
	ans
	(sumsqtrb (rest lst) (+ ans (Math/pow (first lst) 2)))))

(defn sumsqtr [lst]
	(sumsqtrb lst 0))

; 3c - sumsqmr using map and reduce

(defn sumsqmr [lst]
	(reduce + (map square lst)))

; 4 - standard deviation

(defn mean [lst]
	(/ (reduce + lst) (count lst)))

(defn stdev [lst]
	(sqrt (mean (map square (map (fn [x] (- x (mean lst))) lst)))))

; 5 - write tail recursive union and set-difference

(defn unionb [x y answer]
    (if (empty? x)
        (concat answer y) 
        (unionb (rest x) y
	           (if (not (member (first x) y))
		              (cons (first x) answer)
		              answer
		          )
	       )
    )
)
	   

(defn union [x y] (unionb x y (list)))


(defn set-differenceb [x y answer]
    (if (empty? x)
        answer
        (set-differenceb (rest x) y
            (if (member (first x) y)
		answer
                (cons (first x) answer)))))

(defn set-difference [x y] (set-differenceb x y (list)))


; 6 - pascal triangle

(defn addRow [row lst]
    (if (> (length row) 1)
        (addRow (rest row) (cons (+ (first row) (second row)) lst))
	lst))

(defn nextRow [n row]
    (if (> n 0)
	(nextRow (- n 1) (addRow (cons 0 (reverse (cons 0 row))) '()))
	row))

(defn binomial [n] (nextRow n '(1)))

; 7 - maximum value in a binary tree

(defn maxbt [tree]
    (if (cons? tree)
	(max (maxbt(first tree)) (maxbt(rest tree)))
	(if (number? tree)
	    tree
	    -999999)))


; 8 - find variables in expression

(deftrace vars [tree]
    (union 
	       (if (cons? (lhs tree))
	           (vars (lhs tree))
	           (if (not (number? (lhs tree)))
	    				       (list (lhs tree))))
	       (if (cons? (rhs tree))
	           (vars (rhs tree))
	           (if (and (not (number? (rhs tree))) (not (= (rhs tree) nil)))
	    				       (list (rhs tree))))))
; map first alist

; 9 - finds if item occurs somewhere in the tree

(defn occurs [item tree]
    (if (cons? tree)
	(or (occurs item (first tree))
	    (occurs item (rest tree)))
	(if (= item tree)
	    true
	    false)))


; 10 - evaluates tree

(defn myeval [tree]
    (if (not (cons? tree))
	tree
	(let [op (op tree)
	    lhsval (myeval (lhs tree))
	    rhsval (myeval (rhs tree))]
	    (if (= op '+)
		(+ lhsval rhsval)
	        (if (= op '-)
		    (if (= rhsval nil)
			(* lhsval -1)
		        (- lhsval rhsval))
	            (if (= op '*)
		        (* lhsval rhsval)
	                (if (= op '/)
		            (/ lhsval rhsval)
	                    (if (= op 'expt)
		                (expt lhsval rhsval)))))))))

; 11 - evaluates tree where the leaves are either numbers or variables

(defn helper [tree bindings]
    (if (not (cons? tree))
	(if (number? tree)
	    tree
	    (if (symbol? tree)
	        (second (assocl tree bindings))))))


(defn myevalb [tree bindings]
    (if (not (cons? tree))
	(helper tree bindings)
	(let [op (op tree)
	    lhsval (myevalb (lhs tree) bindings)
	    rhsval (myevalb (rhs tree) bindings)]
            (if (= op '+)
		(+ lhsval rhsval)
	        (if (= op '-)
		    (if (= (rhs tree) nil)
			(* lhsval -1)
		        (- lhsval rhsval))
	            (if (= op '*)
		        (* lhsval rhsval)
	                (if (= op '/)
		            (/ lhsval rhsval)
	                    (if (= op 'expt)
		                (expt lhsval rhsval)))))))))


; 12 - converts a tree to java

	
