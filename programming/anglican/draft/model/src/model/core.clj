(ns model.core
  (:use [anglican core runtime emit stat]))

(def width 3)
(def height 3)
(def initial-board [['blue 'red 'green]['blue 'yellow 'red]['yellow 'yellow 'green]])

(defn new-color [color new-color-index]
  (cond
    (= color 'red)
    (cond
      (= new-color-index 0) 'green
      (= new-color-index 1) 'blue
      (= new-color-index 2) 'yellow)
    (= color 'green)
    (cond
      (= new-color-index 0) 'red
      (= new-color-index 1) 'blue
      (= new-color-index 2) 'yellow)
    (= color 'blue)
    (cond
      (= new-color-index 0) 'red
      (= new-color-index 1) 'green
      (= new-color-index 2) 'yellow)
    (= color 'yellow)
    (cond
      (= new-color-index 0) 'red
      (= new-color-index 1) 'green
      (= new-color-index 2) 'blue)))

(defn create-board []
  {:width 3
   :height 3
   :board initial-board
   :score 0})

(let [board (ref (create-board))]
  (println (board :board)))

(with-primitive-procedures [create-board]
  (defquery turntest board
    (println (format "initial board: %s" (board :board)))))

(let [board (ref (create-board))]
  (first (doquery :lmh turntest board)))

(with-primitive-procedures [new-color create-board]
  (defquery turn board
    (println (format "initial board: %s" board))
    (let [position-x (sample (uniform-discrete 0 3))
          position-y (sample (uniform-discrete 0 3))]
        (println (format "position: %d - %d" position-x position-y))
        (let [color (get-in board [position-y position-x])]
          (println (format "color: %s" color))
          (let [new-color (new-color color (sample (discrete (list (/ 1 3) (/ 1 3) (/ 1 3)))))]
            (let [new-board (assoc-in board [position-y position-x] new-color)]
              (println (format "new color: %s" new-color))
              (println (format "new board: %s" new-board))
              (+ 2 5)))))))

(first (doquery :lmh turn initial-board))












(defdist color-change [color] []
  (sample ))


