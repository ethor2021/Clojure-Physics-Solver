; Assignment 3		Erik Thoreson ejt735

; Question 1

(defn solveqns [eqns vals v]
    (if (assocl v vals) 
    				(second (assocl v vals))
        (if (not (cons? eqns))
            nil
            (if (= (length (vars (sublis vals (first eqns)))) 1)
                (let [variable (first (vars (sublis vals (first eqns))))
        								      solved (myevalb (rhs (solve (first eqns) variable)) vals)]
        								    (solveqns (rest eqns) (cons (list variable solved) vals) v))
                (solveqns (rest eqns) vals v)
            )
        )
    )
)


; Question 2

(defn solveqnsc [codelist equations knowns var]
    (if (member var knowns) ;went through all equations without findinf the variable
        codelist
        (if (not (cons? equations))
            nil
            (if (= (length (set-difference (vars (first equations)) knowns)) 1)
        				    (let [variable (first (set-difference (vars (first equations)) knowns))
        								    		solved (solve (first equations) variable)]
        										    (solveqnsc (cons solved codelist) (rest equations) (cons variable knowns) var))
        				    (solveqnsc codelist (rest equations) knowns var)
            )
        )
    )
)

(defn vars [tree]
    (if (symbol? tree)
        tree
        (union 
	           (if (cons? (lhs tree))
	               (vars (lhs tree))
	               (if (not (number? (lhs tree)))
	    				           (list (lhs tree))))
	           (if (cons? (rhs tree))
	               (vars (rhs tree))
	               (if (and (not (number? (rhs tree))) (not (= (rhs tree) nil)))
	    				           (list (rhs tree)))))))

(defn filtercode [codelist needed] ;need to cons on something
    (if (not (cons? codelist))
        '()
        (if (member (lhs (first codelist)) needed)
            (if (symbol? (rhs (first codelist)))
                (cons (first codelist) (filtercode (rest codelist) (union (list (vars (rhs (first codelist)))) needed)))
                (cons (first codelist) (filtercode (rest codelist) (union (vars (rhs (first codelist))) needed)))
            )
            (filtercode (filter (fn [x] (not (equal (first codelist) x))) codelist) needed)
        )
    )
)

; Question 3
(defn tojavah [tree]
    (if (not (cons? tree))
        (str tree)
        (let [op (op tree)
              lhsval (tojavah (lhs tree))
              rhsval (tojavah (rhs tree))]
            (if (= op '-)
                (if (= (rhs tree) nil)
                				(str "(-" lhsval ")")
                				(str "(" lhsval "-" rhsval ")")
                )
                (if (= op '+)
                				(str "(" lhsval "+" rhsval ")")
                				(if (= op '*)
                				    (str lhsval "*" rhsval)
                				    (if (= op '/)
                				    				(str lhsval "/" rhsval)
                				    				(if (= op 'expt)
                				    				    (str "Math.pow(" lhsval "," rhsval ")")
                				    				    (if (= op '=)
                				    				        (str lhsval op rhsval)
                    				    				    (str "Math." op "(" lhsval ")")
                    				    				)
                				    				)
                				    )
                				)
                )
            )
        )
    )
)

(defn tojava [tree]
    (str (tojavah tree) ";")
)

(defn headerh [start inputs]
				(if (cons? inputs)
				    (if (not (cons? (rest inputs)))
				        (headerh (str start "double " (first inputs)) (rest inputs))
												(headerh (str start "double " (first inputs) ", ") (rest inputs))
								)
				    start
				)
)

(defn header [name inputs]
    (str (headerh (str "  public static double " name "( ") inputs) ") {")
)

(defn bodyh [equations]
    (if (not (cons? equations))
        '()
        (cons (str "    double " (tojava (first equations))) (bodyh (rest equations)))
    )
)

(defn body [equations inputs var]
    (bodyh (reverse (filtercode (solveqnsc '() equations inputs var) (list var))))
)

(defn solvecode [name equations inputs var]
    (append (append (append (list (header name inputs)) (body equations inputs var)) (list (str "    return " var ";"))) (list (str "}")))
)

