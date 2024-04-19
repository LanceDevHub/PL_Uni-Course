### Question 1: Substitution 1 (let x (+ 1 2) (let x (* 1 2) (+ x x))
=> (+ (* 1 2) (* 1 2)) (answer 4)

### Question 2: Substitution 2 (let y 3 (let y x (let x 1 (* y x))))
=> (* x 1) (answer 7)

### Question 3: Evaluation 
    (prog
        (fundef f n (let m (+ n 2) (* m 3)))
        (app f (+ 2 1))

(app f (+ 2 1))

subst n := (+ 2 1) => (let m (+ (+ 2 1) 2) (* m 3))

subst m := (+ (+ 2 1) 2) => (* (+ (+ 2 1) 2) 3)

interp expression => (* (+ 3 2) 3) => (* 5 3) => 15


