(ns calaja.client.functions
  (:use [calaja.client.model])
  (:require [calaja.client.model]
            [calaja.client.coordinate :refer :all])
  (:import [calaja.client.model
            Element Player Bullet Game]
           [java.awt.geom AffineTransform Path2D Ellipse2D]))


(defn get-bbox [has-element]
  (.getBounds (-> has-element :element :tshape)))


(defn accelerate

  ([element dt]
     (let [{:keys [velocity angle thrust]} element]
       (update-in element [:velocity] accelerate angle thrust dt)))

  ([velocity angle thrust dt]
     (if (zero? thrust)
       velocity
       (let [dv (* dt thrust)
             ds (polar dv angle)]
         (sum ds velocity)))))


(defn rotate

  ([element dt]
     (let [{:keys [angle spin]} element]
       (update-in element [:angle] rotate spin dt)))

  ([angle spin dt]
     (-> spin (* dt) (+ angle) (rem (* 2 Math/PI)))))


(defn transform

  ([element]
     (let [{:keys [shape point angle]} element
           result (transform shape point angle)]
       (assoc-in element [:tshape] result)))

  ([shape point angle]
     (let [x  (xval point)
           y  (yval point)
           at (AffineTransform.)]
       (.translate at x y)
       (.rotate at (- angle (/ Math/PI 2)))
       (.createTransformedShape at shape))))


(defmulti wrap (fn [x xmax] (class x)))

(defmethod wrap Element [el bounds]
  (update-in el [:point] #(let [[xmax ymax] bounds]
                            (cartesian (wrap (xval %) xmax)
                                       (wrap (yval %) ymax)))))

(defmethod wrap :default [x bound]
  (-> x (rem bound) (+ bound) (rem bound)))


(defn translate

  ([element dt]
     (update-in element [:point] translate (:velocity element) dt))

  ([point velocity dt]
     (let [radius (norm velocity)
           theta  (angle velocity)
           ds     (polar (* dt radius) theta)]
       (sum point ds))))


(defn new-path [xs ys]
  (let [points    (map vector xs ys)
        [x0 y0]   (map first points)
        lines     (rest points)
        path      (java.awt.geom.Path2D$Double.)]
    (.moveTo path x0 y0)
    (doseq [[xn yn] lines] (.lineTo path xn yn))
    (.closePath path)
    path))
