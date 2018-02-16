(ns model.test
  (:use [anglican core runtime emit stat]))

(def red-color-distribution )
(def green-color-distribution )
(def blue-color-distribution )
(def yellow-color-distribution )

(defn get-color-distribution [color]
  (cond
    (= color 0) (list (/ 1 3) (/ 1 3) (/ 1 3))
    (= color 1) (list (/ 1 3) (/ 1 3) (/ 1 3))
    (= color 2) (list (/ 1 3) (/ 1 3) (/ 1 3))
    (= color 3) (list (/ 1 3) (/ 1 3) (/ 1 3))))

(with-primitive-procedures [get-color-distribution remove]
  (defquery change-color color
    (let [new-color-index (sample (discrete (get-color-distribution color)))
          new-colors (remove #{color} [0 1 2 3])]
      (nth new-colors new-color-index))))



(first (doquery :lmh change-color 3))

(defn remove-element-from-vector [vector element acc]
  (let [vector-element (pop vector)]
    if(= vector-element element) ))

