(let x (read)
  (let y (read)
    (seq
      (print (< x y))
      (print (<= x y))
      (print (= x y))
      (print (>= x y))
      (print (> x y))
      (print (!= x y)))))
