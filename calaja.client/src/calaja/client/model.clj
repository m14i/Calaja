(ns calaja.client.model)

(defrecord Element  [point angle velocity thrust spin shape tshape])
(defrecord Player   [name energy shootDelay element])
(defrecord Bullet   [energy alive element])
(defrecord Game     [bounds players bullets])

(defrecord PolarCoordinate     [radius theta])
(defrecord CartesianCoordinate [x y])


(defprotocol Coordinate
  (magnitude [this])
  (cartesian [this])
  (polar [this])
  (sum [this that]))


(defn- plus [& xs]
  (let [cs (map cartesian xs)
        x (apply + (map :x cs))
        y (apply + (map :y cs))]
    (CartesianCoordinate. x y)))


(extend-type PolarCoordinate
  Coordinate

  (magnitude [this]
    (:radius this))

  (cartesian [this]
    (let [{:keys [radius theta]} this
          x (* radius (Math/cos theta))
          y (* radius (Math/sin theta))]
      (CartesianCoordinate. x y)))

  (polar [this]
    this)

  (sum [this that]
    (plus this that)))


(extend-type CartesianCoordinate
  Coordinate

  (magnitude [this]
    (let [{:keys [x y]} this]
      (Math/sqrt (+ (Math/pow x 2)
                    (Math/pow y 2)))))

  (cartesian [this]
    this)

  (polar [this]
    (let [{:keys [x y]} this
          radius (magnitude this)
          theta (Math/atan2 y x)]
      (PolarCoordinate. radius theta)))

  (sum [this that]
    (plus this that)))