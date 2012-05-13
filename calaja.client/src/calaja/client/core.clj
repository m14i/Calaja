(ns calaja.client.core

  (:use [calaja.client.game]
        [calaja.client.render]
        [calaja.client.tools])

  (:import [java.awt RenderingHints Color BasicStroke Toolkit]
           [java.awt.event KeyListener KeyEvent]
           [java.awt.geom AffineTransform Path2D]
           [java.awt Rectangle]
           [javax.swing JFrame SwingUtilities]))


(def game-height 800)
(def game-width 800)
(def game-bounds [game-width game-height])
(def game-center [(/ game-width 2) (/ game-height 2)])
(def game-rendering-hints (RenderingHints.
                            RenderingHints/KEY_ANTIALIASING
                            RenderingHints/VALUE_ANTIALIAS_ON))

(def player-actions {:one {:thrust KeyEvent/VK_UP
                           :left KeyEvent/VK_LEFT
                           :right KeyEvent/VK_RIGHT
                           :shoot KeyEvent/VK_DOWN}

                     :two {:thrust KeyEvent/VK_W
                           :left KeyEvent/VK_A
                           :right KeyEvent/VK_D
                           :shoot KeyEvent/VK_S}})


(def game-delay (atom 20))
(def keys-held (atom #{}))
(def bullets (atom []))
(def players (atom [(new-player :one 1 game-center)
                    (new-player :two 1 (map + game-center [100 100]))]))


(defn rotate-by [keys actions]
  (cond
    (keys (:left actions)) -0.01
    (keys (:right actions)) 0.01
    :else 0))


(defn accelerate-by [keys actions]
  (cond
    (keys (:thrust actions)) 0.0005
    :else 0))


(defn shoot-by [keys actions]
  (cond
    (keys (:shoot actions)) true
    :else false))


(defn process-player-keys [player keys]
  (let [actions (player-actions (:name player))]
    (-> player
      (assoc-in [:element :spin ] (rotate-by keys actions))
      (assoc-in [:element :thrust ] (accelerate-by keys actions))
      (assoc-in [:shoot ] (shoot-by keys actions)))))


(defn process-delay-keys [keys]
  (cond
    (keys KeyEvent/VK_EQUALS) dec
    (keys KeyEvent/VK_MINUS) inc
    :else identity))


(defn render [g]
  (doseq [p @players]
    (draw p g))

  (doseq [b @bullets]
    (draw b g)))


(defn get-bbox [has-element]
  (.getBounds (-> has-element :element :tshape )))

(defn process-hit [player bullets]
  (let [pbox (get-bbox player)
        bboxes (map get-bbox bullets)]
    (if (some #(.intersects pbox %) bboxes)
      (update-in player [:energy ] dec)
      player)))


(defn step [dt]
  (letfn [(step-player [p]
            (-> p
              (process-player-keys @keys-held)
              (move game-bounds dt)
              (process-hit @bullets)))

          (add-bullets [bs p]
            (if (:shoot p)
              (conj bs (shoot p))
              bs))

          (step-bullet [b]
            (-> b
              (move game-bounds dt)
              (update-in [:alive ] #(- % dt))))

          (bullet-alive? [b]
            (< 0 (:alive b)))]

    (swap! game-delay (process-delay-keys @keys-held))

    (swap! players #(map step-player %))

    (swap! bullets #(->> % (filter bullet-alive?) (map step-bullet)))

    (swap! bullets #(reduce add-bullets % @players))))


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
  (SwingUtilities/invokeLater start-game))


