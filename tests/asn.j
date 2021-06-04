(let x 42
  (seq
    (print x)
    (:= x (+ x 1))
    (print x)
    (:= x (+ x 1))
    (print x)))
