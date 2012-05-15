(ns calaja.client.game

  (:use [calaja.client.tools])

  (:import [java.awt.geom AffineTransform Path2D Ellipse2D]))


(def pi-2 (* 2 Math/PI))

(defrecord Element [point angle velocity thrust spin shape tshape])

(defrecord Player [name energy shoot actions element])

(defrecord Bullet [player energy alive element])


(defn to-cartesian [magnitude angle]
  [(-> angle Math/sin - (* magnitude))
   (-> angle Math/cos (* magnitude))])


(defn new-path [xs ys]
  (let [points  (map vector xs ys)
        [x0 y0] (map first points)
        lines   (rest points)
        path    (java.awt.geom.Path2D$Double.)]
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
        v       (to-cartesian 1 (:angle element))
        source  (mapv #(* 30 %) v)
        [x y]   (mapv + source (:point element))
        circle  (java.awt.geom.Ellipse2D$Float. 0 0 5 5)]
    (Bullet. player 1 1000 (Element. [x y] 0 v 0 0 circle circle))))


(defn new-player [name energy point]
  (let [shape (new-ship 10)]
    (Player. name energy false #{} (Element. point 0 [0 0] 0 0 shape shape))))


(defn get-bbox [has-element]
  (.getBounds (-> has-element :element :tshape )))


(defn process-hit [player bullets]
  (let [pbox    (get-bbox player)
        bboxes  (map get-bbox bullets)]
    (if (some #(.intersects pbox %) bboxes)
      (update-in player [:energy ] dec)
      player)))


(defn update-player [player actions]
  (reduce
    #(case %2
       :shoot   (assoc-in %1 [:shoot ] true)
       :thrust  (assoc-in %1 [:element :thrust ] 0.0005)
       :right   (assoc-in %1 [:element :spin ] 0.01)
       :left    (assoc-in %1 [:element :spin ] -0.01)
       %1)
    (-> player
      (assoc-in [:element :spin ] 0)
      (assoc-in [:element :thrust ] 0)
      (assoc-in [:shoot ] false))
    actions))


(defn bullet-alive? [b]
  (< 0 (:alive b)))


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
    (let [{:keys [point velocity]} element]
      (update-in element [:point ] translate velocity dt)))

  ([point velocity dt]
    (let [step (map #(* % dt) velocity)]
      (map + point step))))


(defprotocol IMove
  (move [this limits dt]))

(defprotocol IShoot
  (shoot [this]))


(extend-type Element
  IMove
  (move [this bounds dt]
    (-> this
        (rotate dt)
        (accelerate dt)
        (translate dt)
        (wrap bounds)
        (transform))))


(extend-type Bullet
  IMove
  (move [this limits dt]
    (update-in this [:element ] move limits dt)))


(extend-type Player
  IMove
  (move [this limits dt]
    (update-in this [:element ] move limits dt))

  IShoot
  (shoot [this]
    (new-bullet this)))


(defn step-players [players bullets actions bounds dt]
  (map #(let [[p a] %]
          (-> p
              (update-player a)
              (move bounds dt)
              (process-hit bullets)))
    (map vector players actions)))


(defn step-bullets [bullets players bounds dt]
  (vec (concat
         (->> bullets
              (filter #(< 0 (:alive %)))
              (map #(move (update-in % [:alive ] - dt) bounds dt)))
         (->> players
              (filter :shoot )
              (map shoot)))))