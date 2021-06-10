(let x 7
  (let y 3
    (seq
      (let z 10 (print z))
      (:= y (+ y 1))
      (:= x (+ x 1))
      (+ x y))))
