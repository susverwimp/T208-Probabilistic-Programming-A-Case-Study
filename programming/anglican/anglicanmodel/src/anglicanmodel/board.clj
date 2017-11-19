(ns anglicanmodel.board)

(def board-setup ['r 'r 'y 'r 'b 'r 'b 'r 'r])

(defn initial-board []
  board-setup)

(defn get-color [x y]
  (board-setup (+ (* y 3) x)))