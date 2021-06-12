(let x 10
  (let y 0
    (while (< y x)
      (seq
        (print x)
        (:= y (+ y 1))))))
