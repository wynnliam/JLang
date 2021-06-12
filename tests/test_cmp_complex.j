(let x (read)
  (seq
    (+ 4 5)
    (< x 42)
    (> x 42)
    (= x 42)
    (:= x (>= x 42))
    (<= x 42)
    (print x)))
