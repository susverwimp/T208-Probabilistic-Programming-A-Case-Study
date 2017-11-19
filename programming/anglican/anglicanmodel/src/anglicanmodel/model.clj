(ns anglicanmodel.model
  (:use [anglican core emit runtime stat]
        clojure.repl)
  (:require [anglicanmodel.board :as board]))

(defn show-board []
  (board/initial-board))


(defn board [turn]
  (if (= turn 0) 
    (board/initial-board)
    (recur (- turn 1))))


(defquery one-flip [outcome]
  (let [bias (sample (uniform-continuous 0.0 1.0))]
    (observe (flip bias) outcome)
    bias))

(defquery example
  (let [bet (sample (beta 5 3))]
    (observe (flip bet) true)
    (> bet 0.7)))

(def samples 
  (doquery :rmh example true))

(first samples)