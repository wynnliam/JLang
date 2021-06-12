(let x (read)
  (let cmp (read)
    (let y (if (= x cmp)
              42
              (let z 10 z))
      (print y))))
