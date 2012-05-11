(ns calaja.client.game

  (:use [calaja.client.tools])

  (:import [java.awt.geom AffineTransform Path2D]))


(def TWO_PI (* 2 Math/PI))


(defprotocol IMove
  (move [this dt]))


(defrecord Game [players])


(defrecord Element [point angle velocity spin shape])


(defrecord Player [name energy element])


(defn transform [shape point angle]
  (let [[x y] point]
    (-> (doto (AffineTransform.)
          (.translate x y)
          (.rotate angle))
      (.createTransformedShape shape))))


(defn path [xs ys]
  (let [points (map vector xs ys)
        [x0 y0] (map first points)
        lines (rest points)
        path (java.awt.geom.Path2D$Double.)]
    (.moveTo path x0 y0)
    (doseq [[xn yn] lines] (.lineTo path xn yn))
    (.closePath path)
    path))


(defn ship [size]
  (letfn [(scale [x] (->> 2 Math/sqrt (/ 1) (* size x)))]
    (path
      (map scale [0 3 2 1 0 -1 -2 -3])
      (map scale [3 0 -1 0 -1 0 -1 0]))))


(defn player [name energy point]
  (Player. name energy (Element. point 0 0 0 (ship 20))))


(defn accelerate

  ([element dt]
    (let [{:keys [point angle velocity]} element]
      (update-in element [:point ] accelerate angle velocity dt)))

  ([point angle velocity dt]
    (let [dv (* dt velocity)
          ds [(-> angle Math/sin - (* dv))
              (-> angle Math/cos (* dv))]]
      (map + point ds))))


(defn rotate

  ([element dt]
    (let [{:keys [angle spin]} element]
      (splat element)
      (update-in element [:angle ] rotate spin dt)))

  ([angle spin dt]
    (-> spin (* dt) (+ angle) (rem TWO_PI))))


(extend-type Element
  IMove
  (move [this dt]
    (-> this (rotate dt) (accelerate dt))))


(extend-type Player
  IMove
  (move [this dt]
    (update-in this [:element ] move dt)))

