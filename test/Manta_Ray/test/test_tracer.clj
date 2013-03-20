(ns Manta-Ray.test.test-tracer
  (:use clojure.test)
  (:require [Manta-Ray.vector :as vector])
  (:require [Manta-Ray.tracer :as tracer]))


(defn dprint
  ([o]
    (println (str "Debug output: " o ".\n"))
    o)
  ([s o]
    (println (str s ": " o ".\n"))
    o))

(deftest test-intersect
  (is
    (=
      (tracer/intersect
        [{
          :type :sphere
          :radius 0.5
          :position [0.0, 0.0, 0.0]
          :material :solid-red
        }]
        {
          :origin [1.0, 0.0, 0.0]
          :direction [-1.0, 0.0, 0.0]
        })
      {
        :collision [0.5, 0.0, 0.0]
        :material :solid-red
        :normal [1.0, 0.0, 0.0]
      })))

(deftest test-no-intersect
  (is
    (=
      (tracer/intersect
        [{
          :type :sphere
          :radius 0.5
          :position [0.0, 0.0, 0.0]
          :material :solid-red
        }]
        {
          :origin [1.0, 0.0, 0.0]
          :direction [1.0, 0.0, 0.0]
        })
      nil)))

(deftest test-sphere
  (is
    (=
      0.5
      ((get-in tracer/shape-lib [:sphere :intersect])
        {
          :type :sphere
          :radius 0.5
          :position [0.0, 0.0, 0.0]
          :material :solid-red
        }
        {
          :origin [1.0, 0.0, 0.0]
          :direction [-1.0, 0.0, 0.0]
        })))
  (is
    (<
      ((get-in tracer/shape-lib [:sphere :intersect])
        {
          :type :sphere
          :radius 0.5
          :position [0.0, 0.0, 0.0]
          :material :solid-red
        }
        {
          :origin [1.0, 0.0, 0.0]
          :direction [1.0, 0.0, 0.0]
        })
      0)))

(deftest test-ray-towards
  (let [s {:type :sphere
          :radius 2.0
          :position [-8.2, -4.0, 6.7]
          :material :solid-red}
        o [3.0 -3.7 5.2]]
    (is
      (every?
        (fn [surface] (not (= surface nil)))
        (map
          (fn [ray] (tracer/intersect [s] ray))
          ((get-in tracer/shape-lib [:sphere :rays-toward])
            s
            o
            10))))
    (is
      (every?
        (fn [surface] (= surface nil))
        (map
          (fn [ray]
            (tracer/intersect [s] ray))
          (map
            (fn [ray] (assoc ray :direction (mapv #(* % -1.0) (get ray :direction))))
            ((get-in tracer/shape-lib [:sphere :rays-toward])
              s
              o
              100)))))))

(deftest test-plane)

; (deftest test-sphere)