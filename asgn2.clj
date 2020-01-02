; asgn2		Erik Thoreson EJT735

;Question 1

(defn findpath [item cave]
    (if (= item cave)
	       '()
	       (if (not (cons? cave))
	           nil
	           (let [path (findpath item (first cave))]
		              (if path (cons 'first path)
		                  (let [path (findpath item (rest cave))]
	    		             (if path (cons 'rest path))))))))

(defn follow [path cave]
    (if (not (cons? path))
        cave
	       (if (= (first path) 'first)
	           (follow (rest path) (first cave))
	           (follow (rest path) (rest cave)))))

(defn corresp [item tree1 tree2]
    (follow (findpath item tree1) tree2))


;Question 2

(defn solve [e v]
    (if (= (lhs e) v)
	       e
	       (if (= (rhs e) v)
	           (list (op e) (rhs e) (lhs e))
	            (if (and (not (seq? (rhs e))) (not (= (rhs e) v)))
		               nil
		               (if (seq? (rhs e))
	                (if (= (op (rhs e)) '-)
	                				(if (and (= (rhs (rhs e)) nil) (= (lhs (rhs e)) v))
	                				    (let [sol (solve (list '= (list '- (lhs e))(lhs (rhs e))) v)] (or sol nil))
		                       (let [solRight (solve
		                            (list '= 
		                            (list '+ (lhs e)
		                            (rhs (rhs e)))
		                            (lhs (rhs e)))
		                            v)]
		                         (or solRight (solve 
			                           (list '=
			                           (list '- (lhs (rhs e))
			                           (lhs e))
			                           (rhs (rhs e)))
			                           v))))
		                   (if (= (op (rhs e)) '/)
		                       (let [solRight (solve
			                           (list '=
			                           (list '* (lhs e)
			                           (rhs (rhs e)))
			                           (lhs (rhs e)))
			                           v)]
			                        (or solRight (solve
			                           (list '=
			                           (list '/ (lhs (rhs e))
			                           (lhs e))
			                           (rhs (rhs e)))
			                           v)))
		                       (if (= (op (rhs e)) 'expt)
		                           (let [solRight (solve
		                       					    (list '= (list 'sqrt (lhs e)) (lhs (rhs e))) v)]
		                           				(or solRight nil))
		                           (if (= (op (rhs e)) 'sqrt)
		                               (let [sol (solve 
		                                    (list '=
		                                    (list 'expt (lhs e)
		                                    '2 )(lhs (rhs e))) v)]
		                               					(or sol nil))
		                               (if (= (op (rhs e)) 'log)
		                                   (let [sol (solve 
		                                        (list '=
		                                        (list 'exp (lhs e))
		                                        (lhs (rhs e))) v)]
		                               					    (or sol nil))
		                                   (if (= (op (rhs e)) 'exp)
		                                   (let [sol (solve 
		                                        (list '=
		                                        (list 'log (lhs e))
		                                        (lhs (rhs e))) v)]
		                               					    (or sol nil))
			                                       (let [solRight (solve
				                                           (list '=
				                                           (list (second (assocl (op (rhs e)) opposites)) (lhs e)
				                                           (rhs (rhs e)))
				                                           (lhs (rhs e)))
				                                           v)]
			                                         (or solRight (solve
				                                           (list '=
				                                           (list (second (assocl (op (rhs e)) opposites)) (lhs e)
				                                           (lhs (rhs e)))
				                                           (rhs (rhs e)))
				                                           v))))))))))))))


;Question 3

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
		                              (expt lhsval rhsval)
				                            (if (= op 'sqrt)
				                                (sqrt lhsval)
				                                (if (= op 'exp)
					                                   (exp lhsval)
					                                   (if (= op 'log)
					                                       (log lhsval))))))))))))

;Question 4
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

(defn solveit [equations var values]
				(if (cons? equations)
    				(if (set= (vars (first equations)) (cons var (map first values)))
        				(myevalb (rhs (solve (first equations) var)) values)
            (solveit (rest equations) var values))))


; if set= between vars on current equation and vars on var + values, solve that equation for var and myevalb on that 
; equation with the values assocl