(ns calaja.client.core

  (:use [calaja.client.game]
        [calaja.client.render]
        [calaja.client.tools]
        [clojure.set])

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

(def key-actions {:one {KeyEvent/VK_UP :thrust
                        KeyEvent/VK_LEFT :left
                        KeyEvent/VK_RIGHT :right
                        KeyEvent/VK_DOWN :shoot}

                  :two {KeyEvent/VK_W :thrust
                        KeyEvent/VK_A :left
                        KeyEvent/VK_D :right
                        KeyEvent/VK_S :shoot}})


(def game-delay (atom 20))
(def keys-held  (atom #{}))
(def bullets    (atom []))
(def players    (atom [(new-player :one 1 game-center)
                       (new-player :two 1 (map + game-center [100 100]))]))


(defn get-actions [player key-events]
  (let [events (-> player :name key-actions)]
    (->> events keys set (intersection key-events) (map events))))


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


(defn step [dt]
  (let [actions (map #(get-actions % @keys-held) @players)]
    (swap! game-delay (process-delay-keys @keys-held))
    (swap! players step-players @bullets actions game-bounds dt)
    (swap! bullets step-bullets @players game-bounds dt)))


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
          (try
            (step dt)
            (catch Exception e (.printStackTrace e)))
          (.repaint this)
          (let [elapsed (- (now) ti)
                sleep   (max 2 (- @game-delay elapsed))]
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


