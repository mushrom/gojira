(define test_pred
  (lambda (pred? xs)
    (if (every pred? xs)
      (print "Every element in the set satisfies the predicate.")
      (if (any pred? xs)
        (print "An element exists that satisfies the predicate.")
        (print "No element in the set satisfies the predicate.")))))
