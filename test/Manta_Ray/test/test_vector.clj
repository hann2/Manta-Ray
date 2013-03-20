(ns Manta-Ray.test.test-vector
  (:use clojure.test)
  (:require [Manta-Ray.vector :as vector]))

(deftest test-dot
  (is
    (=
      -19
      (vector/dot
        [-3, 7, -2]
        [-1, -2, 4]))))

(deftest test-length-squared
  (is 
    (=
      62
      (vector/sqr-length
        [-3, 7, -2])))
  (is
    (=
      66.58
      (vector/sqr-length
        [2.1, -7.6, -2.1]))))

(deftest test-is-0?
  (is (vector/is-0? [0, 0, 0]))
  (is (not (vector/is-0? [0.2, 0, 0]))))

(deftest test-cross
  (is
    (=
      [12,-22,-7]
      (vector/cross
        [3, 1, 2]
        [1, -2, 8]))))

(deftest test-scale-to
  (is
    (=
      (vector/scale-to [8.0 0.0 0.0] 2.0)
      [2.0 0.0 0.0]))
  (is
    (=
      (vector/scale-to [0.0 0.0 -0.5] -2.0)
      [0.0 0.0 2.0])))
