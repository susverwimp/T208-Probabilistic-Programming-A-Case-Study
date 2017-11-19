;; gorilla-repl.fileformat = 1

;; **
;;; # The Indian GPA Problem
;;; 
;;; This example was inspired by [Stuart Russell](https://www.cs.berkeley.edu/~russell/) who pointed out that most probabilistic programming systems, _Anglican currently included_, produce the "wrong" answer to this problem.  In short, the problem is: if you observe that a student GPA is exactly @@4.0@@ in a model of transcripts of students from the USA (GPA's from @@0.0@@ to @@4.0@@ and India (GPA's from @@0.0@@ to @@10.0@@) what is the probability that the student is from India?  This problem gets at the heart of measure theoretic problems arising from combining distribution and density, problems not easy to automatically avoid in a probabilistic programming system.  As we know from statistics, given the mixture distribution and given the fact that his/her GPA is _exactly_ @@4.0@@,
;;; the probability that the student is American must be @@1.0@@ (i.e. zero probability that the student is from India).  What this really highlights is the definition of _is_.
;;; Does observing a GPA of @@4.0@@ mean exactly and only @@4.0@@?
;; **

;; @@
(ns indian-gpa
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Partially discrete, partially continuous distributions
;;; 
;;; Let's model this anyway.  Anglican allows complex compositions of distributions conveniently.
;;; 
;;; For example, you can easily represent a partially discrete, partially continuous distribution.  Such a distribution could be used to represent the distribution over GPAs of American students, where their GPAs from the interval @@\left(0.0, 4.0\right)@@ are distributed via Beta distribution (parameters @@\alpha@@ and @@\beta@@),
;;; while some small fraction of students have _exactly_ 0 and 4 as their GPAs.
;;; These latter two points should have separate probability masses @@p_1@@ and @@p_2@@.
;;; 
;;; The density function of this distribution can be written as
;;; 
;;; $$ f\left(x\right) = p_1 \delta x + p_2 \delta \left( x - 4.0 \right) + \left( 1 - p_1 - p_2 \right) \frac{(^x/_4)^{\alpha-1}(1- (^x/_4) )^{\beta-1}} {Beta(\alpha,\beta)}\! $$
;;; 
;;; This distribution could be represented as the following simple probabilistic program (e.g. let @@p_1 = 0.0075, p_2 = 0.0425, \alpha = 8.0, \beta = 2.0@@ (to account for grade inflation).  We can also see what this looks like 
;; **

;; @@
(defdist 
  american-gpa  
  []
  (sample* [this]
           (if (sample* (flip 0.95)) 
             (* 4 (sample* (beta 8 2))) 
             (if (sample* (flip 0.85)) 
               4 
               0))))
(plot/histogram 
  (repeatedly 10000 
              #(sample* (american-gpa))) 
  :bins 50 
  :normalize 
  :probability-density)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"5e5aefb5-4afc-4369-8199-b792f3569a6c","values":[{"x":0.0,"y":0},{"x":0.08000000000000002,"y":0.10374999999999998},{"x":0.16000000000000003,"y":0.0},{"x":0.24000000000000005,"y":0.0},{"x":0.32000000000000006,"y":0.0},{"x":0.4000000000000001,"y":0.0},{"x":0.4800000000000001,"y":0.0},{"x":0.56,"y":0.0},{"x":0.6400000000000001,"y":0.0},{"x":0.7200000000000002,"y":0.0},{"x":0.8000000000000003,"y":0.0},{"x":0.8800000000000003,"y":0.0012499999999999998},{"x":0.9600000000000004,"y":0.0},{"x":1.0400000000000005,"y":0.0},{"x":1.1200000000000006,"y":0.0037499999999999994},{"x":1.2000000000000006,"y":0.0},{"x":1.2800000000000007,"y":0.0037499999999999994},{"x":1.3600000000000008,"y":0.004999999999999999},{"x":1.4400000000000008,"y":0.008749999999999999},{"x":1.520000000000001,"y":0.0062499999999999995},{"x":1.600000000000001,"y":0.008749999999999999},{"x":1.680000000000001,"y":0.021249999999999998},{"x":1.7600000000000011,"y":0.034999999999999996},{"x":1.8400000000000012,"y":0.034999999999999996},{"x":1.9200000000000013,"y":0.04499999999999999},{"x":2.0000000000000013,"y":0.06124999999999999},{"x":2.0800000000000014,"y":0.06499999999999999},{"x":2.1600000000000015,"y":0.10624999999999998},{"x":2.2400000000000015,"y":0.13374999999999998},{"x":2.3200000000000016,"y":0.145},{"x":2.4000000000000017,"y":0.16124999999999998},{"x":2.4800000000000018,"y":0.19249999999999998},{"x":2.560000000000002,"y":0.23999999999999996},{"x":2.640000000000002,"y":0.27999999999999997},{"x":2.720000000000002,"y":0.34874999999999995},{"x":2.800000000000002,"y":0.39249999999999996},{"x":2.880000000000002,"y":0.46124999999999994},{"x":2.960000000000002,"y":0.5162499999999999},{"x":3.0400000000000023,"y":0.585},{"x":3.1200000000000023,"y":0.6462499999999999},{"x":3.2000000000000024,"y":0.7074999999999999},{"x":3.2800000000000025,"y":0.7499999999999999},{"x":3.3600000000000025,"y":0.7412499999999999},{"x":3.4400000000000026,"y":0.8062499999999999},{"x":3.5200000000000027,"y":0.8499999999999999},{"x":3.6000000000000028,"y":0.8362499999999998},{"x":3.680000000000003,"y":0.7387499999999999},{"x":3.760000000000003,"y":0.6749999999999999},{"x":3.840000000000003,"y":0.6049999999999999},{"x":3.920000000000003,"y":0.42499999999999993},{"x":4.000000000000003,"y":0.7525},{"x":4.080000000000003,"y":0}]}],"marks":[{"type":"line","from":{"data":"5e5aefb5-4afc-4369-8199-b792f3569a6c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"5e5aefb5-4afc-4369-8199-b792f3569a6c","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5e5aefb5-4afc-4369-8199-b792f3569a6c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"5e5aefb5-4afc-4369-8199-b792f3569a6c\", :values ({:x 0.0, :y 0} {:x 0.08000000000000002, :y 0.10374999999999998} {:x 0.16000000000000003, :y 0.0} {:x 0.24000000000000005, :y 0.0} {:x 0.32000000000000006, :y 0.0} {:x 0.4000000000000001, :y 0.0} {:x 0.4800000000000001, :y 0.0} {:x 0.56, :y 0.0} {:x 0.6400000000000001, :y 0.0} {:x 0.7200000000000002, :y 0.0} {:x 0.8000000000000003, :y 0.0} {:x 0.8800000000000003, :y 0.0012499999999999998} {:x 0.9600000000000004, :y 0.0} {:x 1.0400000000000005, :y 0.0} {:x 1.1200000000000006, :y 0.0037499999999999994} {:x 1.2000000000000006, :y 0.0} {:x 1.2800000000000007, :y 0.0037499999999999994} {:x 1.3600000000000008, :y 0.004999999999999999} {:x 1.4400000000000008, :y 0.008749999999999999} {:x 1.520000000000001, :y 0.0062499999999999995} {:x 1.600000000000001, :y 0.008749999999999999} {:x 1.680000000000001, :y 0.021249999999999998} {:x 1.7600000000000011, :y 0.034999999999999996} {:x 1.8400000000000012, :y 0.034999999999999996} {:x 1.9200000000000013, :y 0.04499999999999999} {:x 2.0000000000000013, :y 0.06124999999999999} {:x 2.0800000000000014, :y 0.06499999999999999} {:x 2.1600000000000015, :y 0.10624999999999998} {:x 2.2400000000000015, :y 0.13374999999999998} {:x 2.3200000000000016, :y 0.145} {:x 2.4000000000000017, :y 0.16124999999999998} {:x 2.4800000000000018, :y 0.19249999999999998} {:x 2.560000000000002, :y 0.23999999999999996} {:x 2.640000000000002, :y 0.27999999999999997} {:x 2.720000000000002, :y 0.34874999999999995} {:x 2.800000000000002, :y 0.39249999999999996} {:x 2.880000000000002, :y 0.46124999999999994} {:x 2.960000000000002, :y 0.5162499999999999} {:x 3.0400000000000023, :y 0.585} {:x 3.1200000000000023, :y 0.6462499999999999} {:x 3.2000000000000024, :y 0.7074999999999999} {:x 3.2800000000000025, :y 0.7499999999999999} {:x 3.3600000000000025, :y 0.7412499999999999} {:x 3.4400000000000026, :y 0.8062499999999999} {:x 3.5200000000000027, :y 0.8499999999999999} {:x 3.6000000000000028, :y 0.8362499999999998} {:x 3.680000000000003, :y 0.7387499999999999} {:x 3.760000000000003, :y 0.6749999999999999} {:x 3.840000000000003, :y 0.6049999999999999} {:x 3.920000000000003, :y 0.42499999999999993} {:x 4.000000000000003, :y 0.7525} {:x 4.080000000000003, :y 0})}], :marks [{:type \"line\", :from {:data \"5e5aefb5-4afc-4369-8199-b792f3569a6c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"5e5aefb5-4afc-4369-8199-b792f3569a6c\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5e5aefb5-4afc-4369-8199-b792f3569a6c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; In India, however, in most GPAs lie in the range [0.0, 10.0], and for instance could be represented as follows:
;; **

;; @@
(defdist indian-gpa 
  [] 
  (sample* [this]
          (if (sample* (flip 0.99)) 
            (* 10 (sample* (beta 5 5))) 
            (if (sample* (flip 0.1))
              0 
              10))))
(plot/histogram 
  (repeatedly 
    10000 
    #(sample* (indian-gpa))) 
  :bins 50 
  :normalize 
  :probability-density)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac","values":[{"x":0.0,"y":0},{"x":0.20000000000000004,"y":0.004999999999999999},{"x":0.4000000000000001,"y":0.0},{"x":0.6000000000000001,"y":4.999999999999999E-4},{"x":0.8000000000000002,"y":0.0},{"x":1.0000000000000002,"y":0.0024999999999999996},{"x":1.2000000000000002,"y":0.009999999999999998},{"x":1.4000000000000001,"y":0.008999999999999998},{"x":1.6,"y":0.014999999999999996},{"x":1.8,"y":0.021499999999999995},{"x":2.0,"y":0.03849999999999999},{"x":2.2,"y":0.04499999999999999},{"x":2.4000000000000004,"y":0.06449999999999999},{"x":2.6000000000000005,"y":0.09099999999999998},{"x":2.8000000000000007,"y":0.09999999999999998},{"x":3.000000000000001,"y":0.11199999999999997},{"x":3.200000000000001,"y":0.13399999999999998},{"x":3.4000000000000012,"y":0.16599999999999995},{"x":3.6000000000000014,"y":0.18749999999999994},{"x":3.8000000000000016,"y":0.16949999999999996},{"x":4.000000000000002,"y":0.20499999999999996},{"x":4.200000000000002,"y":0.19499999999999995},{"x":4.400000000000002,"y":0.21949999999999995},{"x":4.600000000000002,"y":0.21449999999999994},{"x":4.8000000000000025,"y":0.24199999999999994},{"x":5.000000000000003,"y":0.24799999999999994},{"x":5.200000000000003,"y":0.23449999999999996},{"x":5.400000000000003,"y":0.25299999999999995},{"x":5.600000000000003,"y":0.25449999999999995},{"x":5.800000000000003,"y":0.23449999999999996},{"x":6.0000000000000036,"y":0.19949999999999996},{"x":6.200000000000004,"y":0.19699999999999995},{"x":6.400000000000004,"y":0.19749999999999995},{"x":6.600000000000004,"y":0.15549999999999997},{"x":6.800000000000004,"y":0.13299999999999998},{"x":7.000000000000004,"y":0.11599999999999998},{"x":7.200000000000005,"y":0.10349999999999998},{"x":7.400000000000005,"y":0.09399999999999997},{"x":7.600000000000005,"y":0.07099999999999998},{"x":7.800000000000005,"y":0.060499999999999984},{"x":8.000000000000005,"y":0.05649999999999999},{"x":8.200000000000005,"y":0.03949999999999999},{"x":8.400000000000004,"y":0.028999999999999995},{"x":8.600000000000003,"y":0.016999999999999998},{"x":8.800000000000002,"y":0.006999999999999998},{"x":9.000000000000002,"y":0.004999999999999999},{"x":9.200000000000001,"y":0.003499999999999999},{"x":9.4,"y":0.0019999999999999996},{"x":9.6,"y":4.999999999999999E-4},{"x":9.799999999999999,"y":0.0},{"x":9.999999999999998,"y":0.0},{"x":10.199999999999998,"y":0.04049999999999999},{"x":10.399999999999997,"y":0}]}],"marks":[{"type":"line","from":{"data":"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac\", :values ({:x 0.0, :y 0} {:x 0.20000000000000004, :y 0.004999999999999999} {:x 0.4000000000000001, :y 0.0} {:x 0.6000000000000001, :y 4.999999999999999E-4} {:x 0.8000000000000002, :y 0.0} {:x 1.0000000000000002, :y 0.0024999999999999996} {:x 1.2000000000000002, :y 0.009999999999999998} {:x 1.4000000000000001, :y 0.008999999999999998} {:x 1.6, :y 0.014999999999999996} {:x 1.8, :y 0.021499999999999995} {:x 2.0, :y 0.03849999999999999} {:x 2.2, :y 0.04499999999999999} {:x 2.4000000000000004, :y 0.06449999999999999} {:x 2.6000000000000005, :y 0.09099999999999998} {:x 2.8000000000000007, :y 0.09999999999999998} {:x 3.000000000000001, :y 0.11199999999999997} {:x 3.200000000000001, :y 0.13399999999999998} {:x 3.4000000000000012, :y 0.16599999999999995} {:x 3.6000000000000014, :y 0.18749999999999994} {:x 3.8000000000000016, :y 0.16949999999999996} {:x 4.000000000000002, :y 0.20499999999999996} {:x 4.200000000000002, :y 0.19499999999999995} {:x 4.400000000000002, :y 0.21949999999999995} {:x 4.600000000000002, :y 0.21449999999999994} {:x 4.8000000000000025, :y 0.24199999999999994} {:x 5.000000000000003, :y 0.24799999999999994} {:x 5.200000000000003, :y 0.23449999999999996} {:x 5.400000000000003, :y 0.25299999999999995} {:x 5.600000000000003, :y 0.25449999999999995} {:x 5.800000000000003, :y 0.23449999999999996} {:x 6.0000000000000036, :y 0.19949999999999996} {:x 6.200000000000004, :y 0.19699999999999995} {:x 6.400000000000004, :y 0.19749999999999995} {:x 6.600000000000004, :y 0.15549999999999997} {:x 6.800000000000004, :y 0.13299999999999998} {:x 7.000000000000004, :y 0.11599999999999998} {:x 7.200000000000005, :y 0.10349999999999998} {:x 7.400000000000005, :y 0.09399999999999997} {:x 7.600000000000005, :y 0.07099999999999998} {:x 7.800000000000005, :y 0.060499999999999984} {:x 8.000000000000005, :y 0.05649999999999999} {:x 8.200000000000005, :y 0.03949999999999999} {:x 8.400000000000004, :y 0.028999999999999995} {:x 8.600000000000003, :y 0.016999999999999998} {:x 8.800000000000002, :y 0.006999999999999998} {:x 9.000000000000002, :y 0.004999999999999999} {:x 9.200000000000001, :y 0.003499999999999999} {:x 9.4, :y 0.0019999999999999996} {:x 9.6, :y 4.999999999999999E-4} {:x 9.799999999999999, :y 0.0} {:x 9.999999999999998, :y 0.0} {:x 10.199999999999998, :y 0.04049999999999999} {:x 10.399999999999997, :y 0})}], :marks [{:type \"line\", :from {:data \"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"82b3d4d8-c53c-4929-b00c-08f9b0b4a9ac\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; # Mixture of complex distributions
;;; 
;;; We can easily create the mixture of these two distributions (e.g., the student with the same probability 0.5 either is from some US or Indian university):
;; **

;; @@
(defdist student-gpa 
  [] 
  (sample* [this]
    (if (sample* (flip 0.25))
      (sample* (american-gpa))
      (sample* (indian-gpa)))))
(plot/histogram 
  (repeatedly 
    10000 
    #(sample* (student-gpa)))
  :bins 50 
  :normalize 
  :probability-density)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"f6d67a62-d607-4d98-a7c3-8ef675842c8e","values":[{"x":0.0,"y":0},{"x":0.20000000000000004,"y":0.011999999999999997},{"x":0.4000000000000001,"y":0.0},{"x":0.6000000000000001,"y":0.0},{"x":0.8000000000000002,"y":4.999999999999999E-4},{"x":1.0000000000000002,"y":0.002999999999999999},{"x":1.2000000000000002,"y":0.004499999999999999},{"x":1.4000000000000001,"y":0.010499999999999997},{"x":1.6,"y":0.014499999999999997},{"x":1.8,"y":0.023499999999999993},{"x":2.0,"y":0.04149999999999999},{"x":2.2,"y":0.07299999999999998},{"x":2.4000000000000004,"y":0.08299999999999998},{"x":2.6000000000000005,"y":0.10149999999999998},{"x":2.8000000000000007,"y":0.16299999999999995},{"x":3.000000000000001,"y":0.18049999999999997},{"x":3.200000000000001,"y":0.25399999999999995},{"x":3.4000000000000012,"y":0.31049999999999994},{"x":3.6000000000000014,"y":0.3439999999999999},{"x":3.8000000000000016,"y":0.3399999999999999},{"x":4.000000000000002,"y":0.30499999999999994},{"x":4.200000000000002,"y":0.14399999999999996},{"x":4.400000000000002,"y":0.16299999999999995},{"x":4.600000000000002,"y":0.17399999999999996},{"x":4.8000000000000025,"y":0.18499999999999997},{"x":5.000000000000003,"y":0.17749999999999996},{"x":5.200000000000003,"y":0.18349999999999997},{"x":5.400000000000003,"y":0.18849999999999995},{"x":5.600000000000003,"y":0.17049999999999996},{"x":5.800000000000003,"y":0.17799999999999996},{"x":6.0000000000000036,"y":0.14799999999999996},{"x":6.200000000000004,"y":0.15199999999999997},{"x":6.400000000000004,"y":0.14649999999999996},{"x":6.600000000000004,"y":0.13249999999999998},{"x":6.800000000000004,"y":0.10999999999999997},{"x":7.000000000000004,"y":0.09649999999999997},{"x":7.200000000000005,"y":0.08349999999999998},{"x":7.400000000000005,"y":0.07099999999999998},{"x":7.600000000000005,"y":0.05749999999999999},{"x":7.800000000000005,"y":0.039999999999999994},{"x":8.000000000000005,"y":0.03849999999999999},{"x":8.200000000000005,"y":0.024999999999999994},{"x":8.400000000000004,"y":0.018499999999999996},{"x":8.600000000000003,"y":0.011999999999999997},{"x":8.800000000000002,"y":0.006499999999999999},{"x":9.000000000000002,"y":0.0024999999999999996},{"x":9.200000000000001,"y":0.0014999999999999996},{"x":9.4,"y":4.999999999999999E-4},{"x":9.6,"y":0.0},{"x":9.799999999999999,"y":0.0},{"x":9.999999999999998,"y":0.0},{"x":10.199999999999998,"y":0.028999999999999995},{"x":10.399999999999997,"y":0}]}],"marks":[{"type":"line","from":{"data":"f6d67a62-d607-4d98-a7c3-8ef675842c8e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"f6d67a62-d607-4d98-a7c3-8ef675842c8e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f6d67a62-d607-4d98-a7c3-8ef675842c8e","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"f6d67a62-d607-4d98-a7c3-8ef675842c8e\", :values ({:x 0.0, :y 0} {:x 0.20000000000000004, :y 0.011999999999999997} {:x 0.4000000000000001, :y 0.0} {:x 0.6000000000000001, :y 0.0} {:x 0.8000000000000002, :y 4.999999999999999E-4} {:x 1.0000000000000002, :y 0.002999999999999999} {:x 1.2000000000000002, :y 0.004499999999999999} {:x 1.4000000000000001, :y 0.010499999999999997} {:x 1.6, :y 0.014499999999999997} {:x 1.8, :y 0.023499999999999993} {:x 2.0, :y 0.04149999999999999} {:x 2.2, :y 0.07299999999999998} {:x 2.4000000000000004, :y 0.08299999999999998} {:x 2.6000000000000005, :y 0.10149999999999998} {:x 2.8000000000000007, :y 0.16299999999999995} {:x 3.000000000000001, :y 0.18049999999999997} {:x 3.200000000000001, :y 0.25399999999999995} {:x 3.4000000000000012, :y 0.31049999999999994} {:x 3.6000000000000014, :y 0.3439999999999999} {:x 3.8000000000000016, :y 0.3399999999999999} {:x 4.000000000000002, :y 0.30499999999999994} {:x 4.200000000000002, :y 0.14399999999999996} {:x 4.400000000000002, :y 0.16299999999999995} {:x 4.600000000000002, :y 0.17399999999999996} {:x 4.8000000000000025, :y 0.18499999999999997} {:x 5.000000000000003, :y 0.17749999999999996} {:x 5.200000000000003, :y 0.18349999999999997} {:x 5.400000000000003, :y 0.18849999999999995} {:x 5.600000000000003, :y 0.17049999999999996} {:x 5.800000000000003, :y 0.17799999999999996} {:x 6.0000000000000036, :y 0.14799999999999996} {:x 6.200000000000004, :y 0.15199999999999997} {:x 6.400000000000004, :y 0.14649999999999996} {:x 6.600000000000004, :y 0.13249999999999998} {:x 6.800000000000004, :y 0.10999999999999997} {:x 7.000000000000004, :y 0.09649999999999997} {:x 7.200000000000005, :y 0.08349999999999998} {:x 7.400000000000005, :y 0.07099999999999998} {:x 7.600000000000005, :y 0.05749999999999999} {:x 7.800000000000005, :y 0.039999999999999994} {:x 8.000000000000005, :y 0.03849999999999999} {:x 8.200000000000005, :y 0.024999999999999994} {:x 8.400000000000004, :y 0.018499999999999996} {:x 8.600000000000003, :y 0.011999999999999997} {:x 8.800000000000002, :y 0.006499999999999999} {:x 9.000000000000002, :y 0.0024999999999999996} {:x 9.200000000000001, :y 0.0014999999999999996} {:x 9.4, :y 4.999999999999999E-4} {:x 9.6, :y 0.0} {:x 9.799999999999999, :y 0.0} {:x 9.999999999999998, :y 0.0} {:x 10.199999999999998, :y 0.028999999999999995} {:x 10.399999999999997, :y 0})}], :marks [{:type \"line\", :from {:data \"f6d67a62-d607-4d98-a7c3-8ef675842c8e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"f6d67a62-d607-4d98-a7c3-8ef675842c8e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f6d67a62-d607-4d98-a7c3-8ef675842c8e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; 
;;; # Inferring nationality from GPA example
;;; 
;;; We might like to condition the probabilistic program, inferring the probability of a student being American if his or her GPA is exactly 4.0 but this is syntactically not allowed in Anglican natively (on purpose).
;;; 
;;; However, if we condition on the fact that the student's GPA is appoximately equal to 4.0, so we have some uncertaintly about the full precision (which could be represented via Gaussian noise), the posterior will be different and now there will be some positive probability that student's university is located in India as well.
;; **

;; @@
  (with-primitive-procedures [student-gpa american-gpa indian-gpa] 
    (defquery which-nationality [observed-gpa tolerance]
    (let [nationality (sample (categorical [["USA" 0.25] ["India" 0.75] ]))  
          student_gpa (sample 
                        (if (= nationality "USA") 
                          (american-gpa)
                          (indian-gpa)))]
          (observe (normal student_gpa tolerance) observed-gpa)
           nationality)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;indian-gpa/which-nationality</span>","value":"#'indian-gpa/which-nationality"}
;; <=

;; @@
(take 1 (doquery :smc which-nationality [4.0 tolerance] :number-of-particles 100))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>2.076793740349322</span>","value":"2.076793740349322"}],"value":"[:log-weight 2.076793740349322]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""}],"value":"[:result \"USA\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"}],"value":"{:log-weight 2.076793740349322, :result \"USA\", :predicts []}"}],"value":"({:log-weight 2.076793740349322, :result \"USA\", :predicts []})"}
;; <=

;; @@
(def N 10000)
(def tolerance 0.001)
(def sampler (->> (doquery :smc 
                           which-nationality 
                           [4.0 tolerance] 
                           :number-of-particles 100)
                  (filter #(not= Double/NEGATIVE_INFINITY 
                                 (:log-weight %)))))
(def samples (drop 100 (take (+ N 100) sampler)))
(def num-usa (count (filter #(= % "USA") (map :result samples))))
["USA" (/ num-usa N) "India" (/ (- N num-usa) N) (count samples)]

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""},{"type":"html","content":"<span class='clj-ratio'>1707/2500</span>","value":"1707/2500"},{"type":"html","content":"<span class='clj-string'>&quot;India&quot;</span>","value":"\"India\""},{"type":"html","content":"<span class='clj-ratio'>793/2500</span>","value":"793/2500"},{"type":"html","content":"<span class='clj-unkown'>10000</span>","value":"10000"}],"value":"[\"USA\" 1707/2500 \"India\" 793/2500 10000]"}
;; <=

;; **
;;; By playing with the tolerance we can get different behavior, in particular the time it takes to get the same number of samples will, in general, go up as the tolerance goes down.  Try, for instance, ```(def tolerance 0.0001)``` and notice how much longer it takes while simultaneously getting much more accurate.
;;; 
;;; We can add a Dirac distribution to the language easily enough, effectively setting the toleranace to 0 in the previous query, but in so doing we must be careful, particularly we must realize that inference will effectively default to rejection sampling, and rejection sampling can "not halt."  In this case we should be OK, but only if we ask questions about exactly 0, 4 or 10.  Any other observed GPA values will result in this query never terminating.
;; **

;; @@
; Be careful with this code!   Changing gpa to a value other 
; than 0, 4, or 10 will cause this code not to halt.

(def gpa 4)


(defdist dirac
  "Dirac distribution centered on x"
  [x] []
  (sample* [this] x)
  (observe* [this value] 
            (if (= x value) 
              0.0  
              (- (/ 1.0 0.0)))))

(with-primitive-procedures 
  [student-gpa american-gpa indian-gpa dirac] 
  (defquery which-nationality-with-dirac 
    [observed-gpa]
    (let [nationality (sample (categorical [["USA" 0.6] 
                                            ["India" 0.4] ]))  
          student_gpa (sample 
                        (if (= nationality "USA") 
                          (american-gpa)
                          (indian-gpa)))]
      (observe (dirac student_gpa) observed-gpa)
      nationality)))
(def N 100)

(def sampler (->> 
               (doquery 
                 :smc 
                 which-nationality-with-dirac 
                 [gpa] 
                 :number-of-particles 100)
               (filter #(not= Double/NEGATIVE_INFINITY 
                              (:log-weight %)))))

(def samples (drop 100 (take (+ N 100) sampler)))
(def num-usa (count (filter #(= % "USA") (map :result samples))))
["USA" (float (/ num-usa N)) "India" (float (/ (- N num-usa) N)) "N" N]

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""},{"type":"html","content":"<span class='clj-unkown'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-string'>&quot;India&quot;</span>","value":"\"India\""},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"},{"type":"html","content":"<span class='clj-string'>&quot;N&quot;</span>","value":"\"N\""},{"type":"html","content":"<span class='clj-long'>100</span>","value":"100"}],"value":"[\"USA\" 1.0 \"India\" 0.0 \"N\" 100]"}
;; <=
