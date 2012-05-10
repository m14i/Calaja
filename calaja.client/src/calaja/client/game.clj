(ns calaja.client.game)

(defrecord Element [point angle velocity spin shape])

(defrecord Player [name energy element])

(defprotocol IMove
  (move [this dt]))

(defn mk-path [xs ys]
  (let [points (map vector xs ys)
        [x0 y0] (map first points)
        lines (rest points)
        path (java.awt.geom.Path2D$Double.)]
    (.moveTo path x0 y0)
    (doseq [[xn yn] lines] (.lineTo path xn yn))
    (.closePath path)
    path))

(defn mk-ship [size]
  (letfn [(scale [x] (->> 2 Math/sqrt (/ 1) (* size x)))]
    (mk-path
      (map scale [0 3 2 1 0 -1 -2 -3])
      (map scale [3 0 -1 0 -1 0 -1 0]))))

(defn mk-player [name energy point]
  (Player. name energy (Element. point 0 0 0 (mkShip 20))))

(defn accelerate [dt point angle velocity]
  (let [dv (* dt velocity)
        ds [(-> angle Math/sin - (* dv))
            (-> angle Math/cos (* dv))]]
    (map + point ds)))

(defn rotate [dt angle spin]
  (+ angle (* dt spin)))





(extend Element
  IMove
  (move [this dt]
    (let [da (rotate dt (:angle this) (:spin this))
          dx (accelerate dt (:angle this) (:velocity this) (:point this))
          x (map + (:point this) dx)
          a (+ (:angle this) da)]
      (merge this {:point x :angle a}))))

(extend Player
  IMove
  (move [this dt]
    (move (:element this))))