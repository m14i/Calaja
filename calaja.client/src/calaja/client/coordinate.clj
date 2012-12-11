(ns calaja.client.coordinate)

(deftype PolarCoordinate      [^double r ^double theta])
(deftype CartesianCoordinate  [^double x ^double y])

(defn polar 
  [r theta]
  (PolarCoordinate. r theta))

(defn cartesian 
  [x y]
  (CartesianCoordinate. x y))


(defprotocol Coordinate
  (sum [this that])
  (xval [this])
  (yval [this])
  (norm [this])
  (angle [this]))


(defn- plus [& cs]
  (let [xs (map xval cs)
        ys (map yval cs)
        x (apply + xs)
        y (apply + ys)]
    (CartesianCoordinate. x y)))


(extend-type PolarCoordinate
  Coordinate

  (norm [this]
    (.r this))

  (xval [this]
    (let [radius (.r this)
          theta  (.theta this)]
      (* radius (Math/cos theta))))

  (yval [this]
    (let [radius (.r this)
          theta  (.theta this)]
      (* radius (Math/sin theta))))

  (angle [this]
    (.theta this))

  (sum [this that]
    (plus this that)))


(extend-type CartesianCoordinate
  Coordinate

  (norm [this]
    (let [x (.x this)
          y (.y this)]
      (Math/sqrt (+ (* x x) (* y y)))))

  (xval [this]
    (.x this))

  (yval [this]
    (.y this))

  (angle [this]
    (let [x (.x this)
          y (.y this)]
      (Math/atan2 y x)))

  (sum [this that]
    (plus this that)))
