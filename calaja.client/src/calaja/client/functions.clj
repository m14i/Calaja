(ns calaja.client.functions
  (:use [calaja.client.model])
  (:require [calaja.client.model])
  (:import [calaja.client.model Element Player Bullet Game]
           [java.awt.geom AffineTransform Path2D Ellipse2D]))


(defn to-cartesian [magnitude angle]
  [(-> angle Math/sin - (* magnitude))
   (-> angle Math/cos (* magnitude))])


(defn get-bbox [has-element]
  (.getBounds (-> has-element :element :tshape )))


(defn cap [x xmax]
  (let [xmin (* -1 xmax)]
    (-> x (Math/min xmax) (Math/max xmin))))

(defn accelerate

  ([element dt]
    (let [{:keys [velocity angle thrust]} element]
      (update-in element [:velocity ] accelerate angle thrust dt)))

  ([velocity angle thrust dt]
    (let [dv (* dt thrust)
          ds (to-cartesian dv angle)]
      (map + velocity ds))))


(defn rotate

  ([element dt]
    (let [{:keys [angle spin]} element]
      (update-in element [:angle ] rotate spin dt)))

  ([angle spin dt]
    (-> spin (* dt) (+ angle) (rem (* 2 Math/PI)))))


(defn transform

  ([element]
    (let [{:keys [shape point angle]} element
          result (transform shape point angle)]
      (assoc-in element [:tshape ] result)))

  ([shape point angle]
    (let [[x y] point
          at    (AffineTransform.)]
      (.translate at x y)
      (.rotate at angle)
      (.createTransformedShape at shape))))


(defmulti wrap (fn [x xmax] (class x)))

(defmethod wrap Element [x xmax]
  (update-in x [:point ] #(mapv wrap % xmax)))

(defmethod wrap :default [x xmax]
  (-> x (rem xmax) (+ xmax) (rem xmax)))


(defn translate

  ([element dt]
    (update-in element [:point ] translate (:velocity element) dt))

  ([point velocity dt]
    (let [dx (map #(* % dt) velocity)]
      (map + point dx))))


(defn new-path [xs ys]
  (let [points    (map vector xs ys)
        [x0 y0]   (map first points)
        lines     (rest points)
        path      (java.awt.geom.Path2D$Double.)]
    (.moveTo path x0 y0)
    (doseq [[xn yn] lines] (.lineTo path xn yn))
    (.closePath path)
    path))


