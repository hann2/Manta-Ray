(ns Manta-Ray.core
  (:require [Manta-Ray.tracer :as tracer])
  (:require [Manta-Ray.vector :as vector])
  (:require [clojure.algo.generic.math-functions :as math])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File IOException)))

(defn dprint
  ([o]
    (println (str "Debug output: " o ".\n"))
    o)
  ([s o]
    (println (str s ": " o ".\n"))
    o))

(defn triple-to-int
  [vec]
  (apply
    bit-or
    (map
      #(bit-shift-left
        (int
          (*
            255
            (get vec %)))
        (* 8 (- 2 %)))
      '(0 1 2))))

(defn output-img
  [render, file-name]
  (let [
    img (BufferedImage. (count render) (count (first render)) BufferedImage/TYPE_INT_RGB)]
    (doall
      (map-indexed
        (fn [x col]
          (doall
            (map-indexed
              (fn [y pixel]
                (.setRGB img x y (triple-to-int pixel)))
              col)))
        render))
    (try
      (ImageIO/write img "png" (File. file-name))
      (catch IOException e (.printStackTrace e)))))

(def test-scene
  {
    :camera (tracer/generate-camera [-4 0 -2] [0 0 0] [0, 1, 0], 400, 400, 40, 40)
    :geometry {
      :ball {
        :type :sphere
        :material :solid-white
        :radius 0.5
        :position [0.0 0.0 -1.0]
      }
      :left {
        :type :plane
        :material :solid-blue
        :offset -5.0
        :normal [0.0 0.0 1.0]
      }
      :right {
        :type :plane
        :material :white-light
        :offset -2.0
        :normal [0.0 0.0 -1.0]
      }
      :front {
        :type :plane
        :material :solid-red
        :offset -2.0
        :normal [-1.0 0.0 0.0]
      }
      :back {
        :type :plane
        :material :solid-blue
        :offset -5.0
        :normal [1.0 0.0 0.0]
      }
      :top {
        :type :plane
        :material :solid-blue
        :offset -2.0
        :normal [0.0 1.0 0.0]
      }
      :bottom {
        :type :plane
        :material :solid-green
        :offset -2.0
        :normal [0.0 -1.0 0.0]
      }
   }
  })

; change to :keep-running to run longer
(def control (atom :keep-running))
(defn img-out [] (reset! control :output))
(defn finish [] (reset! control :exit))

;maps one multi-dimensional vector to another multi-dimensional vector calling f on elements at depth dim.
;CURRENTLY ONLY WORKS FOR 3D
(defn dmap
  [f dim & a]
  (doall (apply map (fn [& b] (apply map (fn [& c] (apply map f c)) b)) a)))

;spawns a thread that climbs toward better render. Call (finish) to end the render.
(defn climb
  [scene file-name]
  (future
    (loop [ind 1 img (tracer/raytrace scene)]
      (println (str "Completed iteration " ind ".\n"))
      (let [state @control]
        (if
          (not (= state :keep-running))
          (do
            (output-img (dmap #(/ % ind) 3 img) file-name)
            (compare-and-set! control state :keep-running)))
        (if
          (not (= state :exit))
          (recur (inc ind) (dmap + 3 img (tracer/raytrace scene)))
          (str "Completed image render with " ind " iterations.\n"))))))

(defn go [] (climb test-scene "render.png"))


(defn -main
  []
  (output-img (tracer/raytrace test-scene) "render.png"))

