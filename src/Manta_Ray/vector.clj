(ns Manta-Ray.vector
  (:require [clojure.algo.generic.math-functions :as math]))

;a lot of these functions will be called millions of times so optimize!

(defn dot
  [a b]
  (if (not (= (count a) (count b)))
    (throw (Exception. "Dot product of different length vectors.")))
  (reduce
    +
    (map #(* %1 %2) a b)))

(defn sqr-length
  [vec]
  (dot vec vec))

(defn length
  [vec]
  (math/sqrt (sqr-length vec)))

(defn scale-to
  "scales vec to new length s"
  [vec s]
  (let [l (length vec)]
    (if (= l 0) (throw (Exception. "Cannot scale 0 vector.")))
    (mapv #(* % (/ s l)) vec)))

(defn normalize
  [vec]
  (let [l (length vec)]
    (if (= l 0) (throw (Exception. "Cannot normalize 0 vector.")))
    (mapv #(/ % l) vec)))

(defn is-0?
  [vec]
  (= 0 (sqr-length vec)))

(defn cross
  [a b]
  (if
    (not (and (= (count a) 3) (= (count b) 3)))
    (throw (Exception. "Cross product only works on vectors of length 3.")))
  (if
    (or (is-0? a) (is-0? b))
    ;while technically legal operation, for all my use cases calling cross on 0 vectors will break everything
    (throw (Exception. "Calling cross product on 0 vector.")))
  (if
    (= a b)
    ;while technically legal operation, for all my use cases calling cross on same vectors will break everything
    (throw (Exception. "Calling cross product on same vectors.")))
  (let [a (cycle a) b (cycle b)]
    (vec (for [i [1 2 0]]
         (let [[ay az] (drop i a)
           [by bz] (drop i b)]
         (- (* ay bz)
          (* az by)))))))

(defn ncross
  [a b]
  (normalize (cross a b)))
