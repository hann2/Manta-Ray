(ns Manta-Ray.tracer
  (:require [Manta-Ray.vector :as vector])
  (:require [Manta-Ray.kdtree :as kdtree])
  (:require [clojure.algo.generic.math-functions :as math]))

(defn dprint
  ([o]
    (println (str "Debug output: " o ".\n"))
    o)
  ([s o]
    (println (str s ": " o ".\n"))
    o))

(def material-lib {
    :solid-red {
      :albedo {
        :diffuse [0.75 0.25 0.25]
        :specular [0.0 0.0 0.0]
        :transmitted [0.0 0.0 0.0]
      }
      :exitance [0.0 0.0 0.0]
      :refractive_index 1.0
    }
    :solid-green {
      :albedo {
        :diffuse [0.25 0.75 0.25]
        :specular [0.0 0.0 0.0]
        :transmitted [0.0 0.0 0.0]
      }
      :exitance [0.0 0.0 0.0]
      :refractive_index 1.0
    }
    :solid-blue {
      :albedo {
        :diffuse [0.25 0.25 0.75]
        :specular [0.0 0.0 0.0]
        :transmitted [0.0 0.0 0.0]
      }
      :exitance [0.0 0.0 0.0]
      :refractive_index 1.0
    }
    :solid-white {
      :albedo {
        :diffuse [0.75 0.75 0.75]
        :specular [0.0 0.0 0.0]
        :transmitted [0.0 0.0 0.0]
      }
      :exitance [0.0 0.0 0.0]
      :refractive_index 1.0
    }
    :white-light {
      :albedo {
        :diffuse [0.0 0.0 0.0]
        :specular [0.0 0.0 0.0]
        :transmitted [0.0 0.0 0.0]
      }
      :exitance [0.9 0.9 0.9]
      :refractive_index 1.0
    }
  })

(def irradiance-cache (atom {}))
(def irradiance-vals (atom {}))


;threshold for determining geometric equality
(def threshold 1.0e-2)

;shapes must have evaluate, intersect, and normal functions
;may have surface eczema
(def shape-lib {
    :sphere {
      :evaluate
      (fn [shape location]
        (-
          (math/sqr (get shape :radius))
          (apply + (mapv #(math/sqr (- %1 %2)) (get shape :position) location))))
      :intersect
      (fn [shape ray]
        (let [dist (mapv - (get ray :origin) (get shape :position))
              a (vector/sqr-length (get ray :direction))
              b (* 2 (vector/dot dist (get ray :direction)))
              c (- (vector/sqr-length dist) (math/sqr (get shape :radius)))
              disc (- (math/sqr b) (* 4 a c))]
          (if (< disc 0)
            -1
            ;first and second terms of quadratic formula followed by two solutions, can optimize around sqrt
            (let [f (/ (- b) (* 2 a)) s (/ (math/sqrt disc) (* 2 a)) s1 (+ f s) s2 (- f s)]
              (cond
                (< s1 threshold) s2
                (< s2 threshold) s1
                :else (min s1 s2))))))
      :normal
      (fn [shape location]
        (vector/normalize (mapv - location (get shape :position))))
    }
    :plane {
      :evaluate
      (fn [shape location]
        (- (get shape :offset) (vector/dot (get shape :normal) location)))
      :intersect
      (fn [shape ray]
        (let [denominator (vector/dot (get shape :normal) (get ray :direction))]
          (if (= denominator 0.0)
            -1
            (/
              (-
                (get shape :offset)
                (vector/dot
                  (get shape :normal)
                  (get ray :origin)))
              denominator))))
      :normal
      (fn [shape location]
        (vector/normalize (get shape :normal)))
    }
  })

(defn rand-over
  [f max start end]
  (let [possibility (+ start (rand end))]
    (if
      ;reject possibility 1 - f(possibility)/max of the time
      (> (rand) (/ (f possibility) max))
      possibility
      (rand-over f max start end))))

(defn rays-hemisphere
  [origin dir n]
  (let [
    ;guaranteed to be perpendicular to dir
    x (vector/ncross dir (if (= (get dir 0) 0.0) [1.0 0.0 0.0] [0.0 0.0 1.0]))
    y (vector/cross dir x)]
    (map
      (fn [ind]
        (let [
          theta (rand (* 2 (. Math PI)))
          cos-phi (math/cos (rand-over math/cos 1.0 0.0 (/ (. Math PI) 2.0)))]
          {
            :origin
            origin
            :direction
            (vector/normalize
              (mapv +
              (mapv #(* % (* cos-phi (math/cos theta))) x)
              (mapv #(* % (* cos-phi (math/sin theta))) y)
              (mapv #(* % (math/cos theta)) dir)))
          }))
      (range n))))

(defn generate-camera
  [position, look-toward, up_v, width, height, h-fov, v-fov]
  "Generates a camera given a position, location the camera is looking at, and a head vector"
  ; make them all float vectors
  (let [
    p  (mapv float position)
    la (vector/normalize (mapv - look-toward p))
    u  (vector/normalize (mapv float up_v))
    w  (vector/cross la u)
    offset (fn [fov, vec]
      (let [c (math/tan (/ (* (. Math PI) fov) 360.0))]
        (mapv #(* % c) vec)))]
    {
      :position p
      :look_at la
      ;right wing
      :wing_v w
      :up_v u
      :width width
      :height height
      :h-fov h-fov
      :v-fov v-fov
      ; used for calculating pixel locations C equivalent:
        ;x_offset = scale_mat(scene->camera->wing_v, tan((PI*H_FOV)/360.0));
      :x-offset (offset h-fov w)
      :y-offset (offset v-fov u)
    }))


(defn intersect
  [geometry ray]
  (let [
    close-s
    (apply
      min-key
      (fn [surface] (let [dist (get surface :dist)]
        (if
          (< dist threshold)
          ;negative treated as infinitely far
          1000000
          dist)))
      (map
        (fn [shape]
          {
            :shape shape
            :dist
            ((get-in shape-lib [(get (get geometry shape) :type) :intersect])
              (get geometry shape)
              ray)
          })
        (keys geometry)))
    loc
    (mapv +
      (mapv
        #(* % (get close-s :dist))
        (get ray :direction))
      (get ray :origin))
    shape-key
    (get close-s :shape)
    shape
    (get geometry shape-key)]
    (if
      (< (get close-s :dist) 0)
      nil
      {
        :collision loc
        :material (get shape :material)
        :normal ((get-in shape-lib [(get shape :type) :normal]) shape loc)
        :shape shape-key
      })))

;forward declared so I can use it in shade.
(def components {})

(defn assert-color
  [color]
  (if (not (= 3 (count color)))
    (throw (Exception. (str "Color must be length 3: " color)))
    (if (some #(or (>= % 1.0) (< % 0.0)) color)
      (throw (Exception. (str "A color component is greater than 1.0 or less than 0.0.  Color is: " color)))
      color)))

(defn shade
  [geometry ray weight & {:keys [surface] :or {surface (intersect geometry ray)}}]
  (if
    surface
    (let [
      color (mapv +
        (get-in
          material-lib
          [(get surface :material) :exitance])
        (apply mapv +
          ;seq of 3 vecs
          (mapv
            (fn [component]
              (mapv *
                ((get components component) geometry ray surface weight)
                (get-in material-lib [(get surface :material) :albedo component])))
            (keys components))))]
      (assert-color color))
    [0.0, 0.0, 0.0]))

(defn average-vecs
  [vecs]
  (let [n (count vecs)]
    (mapv
      #(/ % n)
      (reduce (fn [a b] (mapv + a b)) vecs))))

(defn diffuse-diffuse-ray
  [geometry surface-from ray weight]
  (assert-color
    (let [surface-to (intersect geometry ray)]
      (if (= surface-to nil)
        [0.0 0.0 0.0]
        (let [
          scale
          (/
              (vector/dot (get surface-from :normal) (get ray :direction))
              (. Math PI))]
          (mapv
            (fn [element]
              (*
                element
                (cond
                  (< scale 0.0)
                  0.0
                  (> scale 1.0)
                  1.0
                  :else
                  scale)))
            (shade geometry ray weight :surface surface-to)))))))

(defn path-trace
  [geometry surface num-rays weight]
  (let [new-weight
    (* weight
      (apply max
        (get-in
          material-lib
          [(get surface :material) :albedo :diffuse])))]
    (average-vecs
      (map
        #(diffuse-diffuse-ray geometry surface % new-weight)
        (rays-hemisphere
          (get surface :collision)
          (get surface :normal)
          num-rays)))))

(def components {
    :diffuse
    (fn
      [geometry ray surface weight]
      (if (> (rand) weight)
        [0.0 0.0 0.0]
        (let [
          cache @irradiance-cache
          best-val (get (kdtree/nearest-neighbor (get cache (get surface :shape)) (get surface :collision)) :point)]
          (if
            (and
              best-val
              (>
                0.05
                (vector/sqr-length
                  (mapv
                    -
                    best-val
                    (get surface :collision)))))
            (get-in @irradiance-vals [(get surface :shape) best-val])
            (let [color (path-trace geometry surface 8 weight)]
              (swap!
                irradiance-vals
                #(assoc-in
                  %
                  [(get surface :shape) (get surface :collision)]
                  color))
              (swap!
                irradiance-cache
                #(assoc
                  %
                  (get surface :shape)
                  (kdtree/insert
                    (get % (get surface :shape))
                    (get surface :collision))))
              color)))))
    :specular (fn [geometry ray surface weight] [0.0, 0.0, 0.0])
    :transmitted (fn [geometry ray surface weight] [0.0, 0.0, 0.0])
  })

(defn get-pixel
  [scene x y]
  (shade
    (get scene :geometry)
    (let [camera (get scene :camera)
      width (get camera :width)
      height (get camera :height)
      x-scale (/ (- (* x 2.0) width) width)
      y-scale (/ (- (* y 2.0) height) height)]
      {
        :origin (get camera :position)
        :direction
        (vector/normalize
          (mapv +
            (mapv #(* % x-scale) (get camera :x-offset))
            (mapv #(* % y-scale) (get camera :y-offset))
            (get camera :look_at)))
      })
    1.0))

(defn raytrace
  [scene]
  ;initialize irradiance cache
  (doall
    (map
      (fn [key] (reset! irradiance-cache (assoc @irradiance-cache key nil)))
      (get scene :geometry)))
  (pmap
    ;function take x and returns seq of seqs representing column
    (fn [x]
        (pmap
          ;fn takes y and returns seq representing color triple
          (fn [y]
            (get-pixel scene x y))
          (range (get-in scene [:camera :height]))))
    (range (get-in scene [:camera :width]))))


