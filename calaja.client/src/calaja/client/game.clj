(ns calaja.client.game

  (:use [calaja.client.tools])

  (:import [java.awt.geom AffineTransform Path2D]))


(def pi-2 (* 2 Math/PI))

(defrecord Game [players])

(defrecord Element [point angle velocity spin shape tshape])

(defrecord Player [name energy element])


(defn collision? [shape1 shape2]
  (let [bbox1 (.getBounds shape1)
        bbox2 (.getBounds shape2)]
    (.intersects bbox1 bbox2)))


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
      (map scale [3 0 -1 0 -1 0 -1 0]))))


(defn new-player [name energy point]
  (let [shape (new-ship 10)]
    (Player. name energy (Element. point 0 0 0 shape shape))))


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
      (update-in element [:angle ] rotate spin dt)))

  ([angle spin dt]
    (-> spin (* dt) (+ angle) (rem pi-2))))


(defn transform

  ([element]
    (let [{:keys [shape point angle]} element
          result (transform shape point angle)]
      ;(splat result "after transform: " result)
      (assoc-in element [:tshape ] result)))

  ([shape point angle]
    (let [[x y] point
          result (-> (doto (AffineTransform.)
                       (.translate x y)
                       (.rotate angle))
        (.createTransformedShape shape))]
      result)))


(defmulti wrap (fn [x xmax] (class x)))

(defmethod wrap Element [x xmax]
  (update-in x [:point ] #(mapv wrap % xmax)))

(defmethod wrap :default [x xmax]
  (-> x (rem xmax) (+ xmax) (rem xmax)))


(defprotocol IMove
  (move [this limits dt]))


(extend-type Element
  IMove
  (move [this limits dt]
    (-> this
      (rotate dt)
      (accelerate dt)
      (wrap limits)
      (transform))))


(extend-type Player
  IMove
  (move [this limits dt]
    (update-in this [:element ] move limits dt)))

