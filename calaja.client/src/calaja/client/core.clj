(ns calaja.client.core

  (:use [calaja.client.game]
        [calaja.client.render]
        [calaja.client.tools])

  (:import [java.awt RenderingHints Color BasicStroke Toolkit]
           [java.awt.event KeyListener KeyEvent]
           [java.awt.geom AffineTransform Path2D]
           [javax.swing JFrame]))



(def _height 800)
(def _width 800)
(def _delay 20)
(def _keys-held (atom #{}))
(def _hints (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))
(def _center [(/ _width 2) (/ _height 2)])


(def _player (atom (player "player" 1 _center)))


(defn render [g]
  (draw @_player g))


(defn rotate-by [keys]
  (cond
    (keys KeyEvent/VK_LEFT) -0.0001
    (keys KeyEvent/VK_RIGHT) 0.0001
    :else 0))


(defn accelerate-by [keys]
  (cond
    (keys KeyEvent/VK_UP) 0.0005
    :else 0))


(defn process-keys [player keys]
  (-> (splat player)
    (update-in [:element :spin ] + (rotate-by keys))
    (update-in [:element :velocity ] + (accelerate-by keys))))


(defn step [dt]
  (swap! _player #(-> % (process-keys @_keys-held) (move dt))))


(defn now []
  (System/currentTimeMillis))


(defn init-canvas []

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
  (let [canvas (init-canvas)]
    (doto canvas
      (.setFocusable true)
      (.addKeyListener canvas)
      (.setSize _width _height)
      (.setVisible true)
      (.createBufferStrategy 2))))


(defn -main [& args]
  (show))


