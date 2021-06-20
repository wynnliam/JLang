(let x (read)
  (let y (read)
    (let z
      (+ (if (= x y) 42 0) (if (> x y) 5 0))
      (print z))))
