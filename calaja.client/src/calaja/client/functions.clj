(ns calaja.client.functions
  (:use [calaja.client.model])
  (:require [calaja.client.model])
  (:import [calaja.client.model Element Player Bullet Game]
           [java.awt.geom AffineTransform Path2D Ellipse2D]))


(defn splat [x & xs]
  (do
    (apply println xs)
    x))


(defn to-cartesian [magnitude angle]
  [(-> angle Math/sin - (* magnitude))
   (-> angle Math/cos (* magnitude))])


(defn get-bbox [has-element]
  (.getBounds (-> has-element :element :tshape )))


(defn hit? [player bullet]
  (let [pbox (get-bbox player)
        bbox (get-bbox bullet)]
    (.intersects pbox bbox)))


(defn process-hit [player bullets]
  (let [hits (map #(if (hit? player %) (update-in % [:alive ] dec) %) bullets)
        num-hits (reduce 0 #(if (-> %2 :alive zero?) (inc %1) %1) hits)]
    [(if (zero? num-hits)
       player
       (update-in player [:energy ] #(- % num-hits)))
     hits]))


(defn update-player [player actions]
  (reduce
    #(case %2
       :shoot (assoc-in %1 [:shoot ] true)
       :thrust (assoc-in %1 [:element :thrust ] 0.0005)
       :right (assoc-in %1 [:element :spin ] 0.01)
       :left (assoc-in %1 [:element :spin ] -0.01)
       %1)
    (-> player
      (assoc-in [:element :spin ] 0)
      (assoc-in [:element :thrust ] 0)
      (assoc-in [:shoot ] false))
    actions))


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
    (let [[x y] point]
      (-> (doto (AffineTransform.)
            (.translate x y)
            (.rotate angle))
        (.createTransformedShape shape)))))


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
  (let [points (map vector xs ys)
        [x0 y0] (map first points)
        lines (rest points)
        path (java.awt.geom.Path2D$Double.)]
    (.moveTo path x0 y0)
    (doseq [[xn yn] lines] (.lineTo path xn yn))
    (.closePath path)
    path))


(defn new-ship [size]
  (letfn [(scale [x] (->> 2 Math/sqrt (/ 1) (* size x)))]
    (new-path
      (map scale [0 3 2 1 0 -1 -2 -3])
      (map scale [2 -1 -2 -1 -2 -1 -2 -1]))))


(defn new-bullet [player]
  (let [element (:element player)
        v (to-cartesian 1 (:angle element))
        source (mapv #(* 30 %) v)
        [x y] (mapv + source (:point element))
        circle (java.awt.geom.Ellipse2D$Float. 0 0 5 5)]
    (Bullet. player 1 1000 (Element. [x y] 0 v 0 0 circle circle))))


(defn new-player [name energy point]
  (let [shape (new-ship 10)]
    (Player. name energy false nil (Element. point 0 [0 0] 0 0 shape shape))))


