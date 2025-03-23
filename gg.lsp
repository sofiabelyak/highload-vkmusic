(setf *random-state* (make-random-state t)) 

(defun add-numbers (x y)
  (+ x y))

(defun is-winning-sum (x y)
  (or (eq (add-numbers x y) 7) (eq (add-numbers x y) 11)))

(defun has-second-chance (x y)
  (or (and (eq x 1) (eq y 1)) (and (eq x 6) (eq y 6))))

(defun roll-die ()
  (+ (random 6) 1))

(defun roll-dice ()
  (list (roll-die) (roll-die)))

(defun take-turn (player)
  (let* ((dice (roll-dice))
         (x (first dice))
         (y (second dice))
         (sum-dice (add-numbers x y)))
    (print (list 'player player 'rolled dice 'sum sum-dice))
    (cond
      ((is-winning-sum x y)
       (list player dice))
      ((has-second-chance x y)
       (let ((new-dice (roll-dice)))
         (print (list 'player player 'rolled-again new-dice 'sum (add-numbers (first new-dice) (second new-dice))))
         (list player new-dice)))
      (t
       (list player dice)))))

(defun play-game ()
  (let* ((player1 (take-turn 1))
         (x1 (first (second player1)))
         (y1 (second (second player1))))
    (if (is-winning-sum x1 y1)
        (print 'winner-player-1)
        (let* ((player2 (take-turn 2)) 
               (x2 (first (second player2)))
               (y2 (second (second player2))))
          (if (is-winning-sum x2 y2)
              (print 'winner-player-2)
              (if (> (add-numbers x1 y1) (add-numbers x2 y2))
                  (print (list 'winner-player-1 'score (second player1) 'vs (second player2)))
                  (print (list 'winner-player-2 'score (second player2) 'vs (second player1)))))))))

(play-game)