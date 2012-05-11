(ns calaja.client.core

  (:use [calaja.client.game]
        [calaja.client.render]
        [calaja.client.tools])

  (:import [java.awt RenderingHints Color BasicStroke Toolkit]
           [java.awt.event KeyListener KeyEvent]
           [java.awt.geom AffineTransform Path2D]
           [java.awt Rectangle]
           [javax.swing JFrame]))


(def game-height 800)
(def game-width 800)
(def game-rectangle (Rectangle. 0 0 game-width game-height))
(def game-center [(/ game-width 2) (/ game-height 2)])
(def game-rendering-hints (RenderingHints.
                            RenderingHints/KEY_ANTIALIASING
                            RenderingHints/VALUE_ANTIALIAS_ON))

(def player-actions {:one {:thrust KeyEvent/VK_UP
                           :left KeyEvent/VK_LEFT
                           :right KeyEvent/VK_RIGHT}

                     :two {:thrust KeyEvent/VK_W
                           :left KeyEvent/VK_A
                           :right KeyEvent/VK_D}})


(def game-delay (atom 2000))
(def keys-held (atom #{}))
(def players [(atom (new-player :one 1 game-center))
              (atom (new-player :two 1 (map + game-center [100 100])))])


(defn rotate-by [keys actions]
  (cond
    (keys (:left actions)) -0.0001
    (keys (:right actions)) 0.0001
    :else 0))


(defn accelerate-by [keys actions]
  (cond
    (keys (:thrust actions)) 0.0005
    :else 0))


(defn process-player-keys [player keys]
  (let [actions (player-actions (:name player))]
    (-> player
      (update-in [:element :spin ] + (rotate-by keys actions))
      (update-in [:element :velocity ] + (accelerate-by keys actions)))))


(defn process-delay-keys [keys]
  (cond
    (keys KeyEvent/VK_EQUALS) dec
    (keys KeyEvent/VK_MINUS) inc
    :else identity))


(defn render [g]
  (doseq [p players]
    (draw @p g)))


(defn on-canvas? [element]
  (let [bbox (.getBounds (:shape element))]
    (splat nil bbox game-rectangle)
    (.contains game-rectangle bbox)))


(defn step [dt]
  (do
    (swap! game-delay (process-delay-keys @keys-held))
    (doseq [p players]
      (swap! p #(let [p0 (process-player-keys % @keys-held)
                      p1 (move p0 dt)]
                  ;(if (on-canvas? (:element p1))
                  p1
                  ;(move p0 (- dt))
                  )))))


(defn new-canvas []

  (proxy [JFrame KeyListener Runnable] []

    (paint [g]
      (proxy-super paint g)
      (.setRenderingHints g game-rendering-hints)
      (render g))

    (keyTyped [_])

    (keyReleased [ev]
      (swap! keys-held disj (.getKeyCode ev)))

    (keyPressed [ev]
      (swap! keys-held conj (.getKeyCode ev)))

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
                sleep (max 2 (- @game-delay elapsed))]
            (Thread/sleep sleep)
            (recur tj)))))))


(defn start-game []
  (let [canvas (new-canvas)]
    (doto canvas
      (.setFocusable true)
      (.addKeyListener canvas)
      (.setSize game-width game-height)
      (.setVisible true)
      (.createBufferStrategy 2))))


(defn -main [& args]
  (start-game))


