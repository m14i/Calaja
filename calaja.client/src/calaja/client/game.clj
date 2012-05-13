(ns calaja.client.game

  (:use [calaja.client.tools])

  (:import [java.awt.geom AffineTransform Path2D]))


(def pi-2 (* 2 Math/PI))

(defrecord Game [players])

(defrecord Element [point angle velocity thrust spin shape tshape])

(defrecord Player [name energy element])


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


(defn new-player [name energy point]
  (let [shape (new-ship 10)]
    (Player. name energy (Element. point 0 [0 0] 0 0 shape shape))))


(defn to-cartesian [magnitude angle]
  [(-> angle Math/sin - (* magnitude))
   (-> angle Math/cos (* magnitude))])


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


(defn translate

  ([element dt]
    (let [{:keys [point velocity]} element]
      (update-in element [:point ] translate velocity dt)))

  ([point velocity dt]
    (let [step (map #(* % dt) velocity)]
      (map + point step))))


(defprotocol IMove
  (move [this limits dt]))


(extend-type Element
  IMove
  (move [this bounds dt]
    (-> this
      (rotate dt)
      (accelerate dt)
      (translate dt)
      (wrap bounds)
      (transform))))


(extend-type Player
  IMove
  (move [this limits dt]
    (update-in this [:element ] move limits dt)))

