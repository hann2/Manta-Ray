;; Copyright (C) 2009-2012 Brendan Ribera. All rights reserved.

;; The MIT License

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(ns Manta-Ray.kdtree)

(defrecord Node [left right #^doubles value #^long depth])
(defrecord Result [point dist-squared])

(defn- dist-squared [a b]
  "Compute the K-dimensional distance between two points"
  (reduce + (for [i (range (count a))]
              (let [v (- (nth a i) (nth b i))]
                (* v v)))))

(defn build-tree
  "Construct a Kd-tree from points. Assumes all
points are of the same dimension."
  ([points]
     (build-tree points 0))
  ([points depth]
     (let [point-count (count points)]
       (if (= 0 point-count) nil
           (let [k (count (nth points 0))
                 dimension (mod depth k)
                 points (vec (sort-by #(nth % dimension) points))
                 median (quot point-count 2)
                 left-tree (build-tree
                            (subvec points 0 median)
                            (inc depth))
                 right-tree (build-tree
                             (subvec points (inc median))
                             (inc depth))]
             (Node. left-tree
                    right-tree
                    (into-array Double/TYPE (nth points median))
                    depth
                    (meta (nth points median))
                    nil))))))

(defn insert
  "Adds a point to an existing tree."
  ([tree point] (insert tree point 0))
  ([tree point depth]
     (let [k (count point)
           dimension (mod depth k)]
      (if (nil? tree)
        (Node. nil nil (into-array Double/TYPE point) depth)
        (if (< (nth point dimension) (nth (:value tree) dimension))
          (Node.
           (insert (:left tree) point (inc depth))
           (:right tree)
           (:value tree)
           (:depth tree))
          (Node.
           (:left tree)
           (insert (:right tree) point (inc depth))
           (:value tree)
           (:depth tree)))))))

(defn find-min
  "Locate the point with the smallest value in a given dimension.
Used internally by the delete function. Runs in O(√n) time for a
balanced tree."
  ([tree dimension] (find-min tree dimension 0))
  ([tree dimension depth]
     (if (identity tree)
       (let [k (count (:value tree))]
         (if (= dimension (mod depth k))
           ;; if we're at the dimension of interest, follow the left branch or
           ;; take the value - left is always smaller in the currend dimension
           (if (nil? (:left tree))
             (:value tree)
             (find-min (:left tree) dimension (inc depth)))
           ;; otherwise, compare min of self & children
           (first
            (sort-by #(nth % dimension)
             (filter identity
              (list (:value tree)
                    (find-min (:left tree) dimension (inc depth))
                    (find-min (:right tree) dimension (inc depth)))))))))))

(defn delete
  "Delete value at the given point. Runs in O(log n) time for a balanced tree."
  ([tree point] (delete tree point 0))
  ([tree point depth]
     (if (identity tree)
       (let [k (count (:value tree))
             dimension (mod depth k)]
         (cond
          ;; point is to the left
          (< (nth point dimension)
             (nth (:value tree) dimension))
          (Node.
           (delete (:left tree) point (inc depth))
           (:right tree)
           (:value tree)
           (:depth tree))
          
          ;; point is to the right
          (> (nth point dimension)
             (nth (:value tree) dimension))
          (Node.
           (:left tree)
           (delete (:right tree) point (inc depth))
           (:value tree)
           (:depth tree))

          ;; point is here... three cases:
          
          ;; leaf node - delete case, so return nil
          (= nil (:left tree) (:right tree))
          nil

          ;; right is not null.
          (not (nil? (:right tree)))
          (let [value (find-min (:right tree) dimension (inc depth))]
            (Node.
             (:left tree)
             (delete (:right tree) value (inc depth))
             value
             (:depth tree)))

          ;; right is null, left must not be.
          true
          (let [value (find-min (:left tree) dimension (inc depth))]
            (Node.
             nil
             (delete (:left tree) value (inc depth))
             value
             (:depth tree))))))))


(defn nearest-neighbor
  "Compute n nearest neighbors for a point. If n is
omitted, the result is the nearest neighbor;
otherwise, the result is a list of length n."
  ([tree point] (first (nearest-neighbor tree point 1 0 nil)))
  ([tree point n] (nearest-neighbor tree point n 0 nil))
  ([tree point n depth best]
     (if ;; Empty tree? The best list is unchanged.
         (nil? tree) best

         ;; Otherwise, recurse!
         (take n
          (sort-by :dist-squared
           (let [dimension (mod depth (count point))
                 dim-dist (- (nth point dimension)
                             (nth (:value tree) dimension))
                 search-order (if (> dim-dist 0)
                                (list :right :left)
                                (list :left :right))

                 ;; Compute best list for the near-side of the search order
                 best-near (nearest-neighbor
                            ((first search-order) tree)
                            point
                            n
                            (inc depth)
                            (cons
                             (Result. (vec (:value tree))
                                      (dist-squared (:value tree) point)
                                      (meta tree)
                                      nil)
                             best))]
             
             ;; If the square distance of our search node to point in the
             ;; current dimension is still better than the *worst* of the near-
             ;; side best list, there may be a better solution on the far
             ;; side. Compute & combine with near-side solutions.
             (if (< (* dim-dist dim-dist) (:dist-squared (last best-near)))
               (concat best-near
                       (nearest-neighbor
                        ((last search-order) tree) point n (inc depth) nil))
               best-near)))))))