(ns calaja.client.core
  (:import (java.awt RenderingHints Color BasicStroke Toolkit)
           (java.awt.event KeyListener KeyEvent)
           (java.awt.geom AffineTransform Path2D)
           (javax.swing JFrame)))

(def _height 800)
(def _width 800)
(def _delay 20)
(def _keys-held (atom #{}))
(def _hints (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))
(def _center [(/ _width 2) (/ _height 2)])

(defn transform [shape loc rot]
  (let [[x y] loc]
    (-> (doto (AffineTransform.)
          (.translate x y)
          (.rotate rot))
      (.createTransformedShape shape))))

(defprotocol ISprite
  (draw [this g]))

(defrecord Bullet [shape owner loc vel]
  ISprite
  (draw [this g]
    (.draw g (transform (:shape this) (:loc this) 0))))

(defrecord Player [shape name lives shield loc rot vel]
  ISprite
  (draw [this g]
    (.draw g (transform (:shape this) (:loc this) (:rot this)))))

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

(def _player (atom (Player. (mk-ship 20) "player" 3 3 _center 0 [0 0])))
(def _bullets (atom []))

(defn render [g]
  (draw @_player g))

(defn rotate-by [keys]
  (cond
    (keys KeyEvent/VK_LEFT) -0.1
    (keys KeyEvent/VK_RIGHT) 0.1
    :else 0))

(defn accelerate-by [keys]
  (cond
    (keys KeyEvent/VK_UP) 0.0005
    :else 0))

(defn accelerate [v rot a]
  (let [[vx vy] v
        dvx (-> rot Math/sin - (* a))
        dvy (-> rot Math/cos (* a))]
    [(+ vx dvx) (+ vy dvy)]))

(defn process-keys [keys player]
  (-> player
    (update-in [:rot ] + (rotate-by keys))
    (update-in [:vel ] accelerate (:rot player) (accelerate-by keys))))

(defn move [dt sprite]
  (let [v (:vel sprite)
        ds (map (partial * dt) v)]
    (update-in sprite [:loc ] #(map + ds %))))

(defn step [dt]
  (swap! _player #(->> %
                    (process-keys @_keys-held)
                    (move dt))))

(defn now []
  (System/currentTimeMillis))

(defn mk-canvas []
  (proxy [JFrame KeyListener Runnable] []
    (paint [g]
      (proxy-super paint g)
      (.setRenderingHints g _hints)
      (render g))
    (keyTyped [_])
    (keyReleased [ev]
      (swap! _keys-held disj (.getKeyCode ev)))
    (keyPressed [ev]
      (swap! _keys-held conj (.getKeyCode ev)))
    (addNotify []
      (proxy-super addNotify)
      (-> (Thread. this) .start))
    (run []
      (loop [ti (now)]
        (let [tj (now)
              dt (- tj ti)]
          (step dt)
          (.repaint this)
          (let [elapsed (- (now) ti)
                sleep (max 2 (- _delay elapsed))]
            (Thread/sleep sleep)
            (recur tj)))))))

(defn show []
  (let [canvas (mk-canvas)]
    (doto canvas
      (.setFocusable true)
      (.addKeyListener canvas)
      (.setSize _width _height)
      (.setVisible true)
      (.createBufferStrategy 2))))

(defn -main [& args]
  (show))


