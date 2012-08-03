(ns calaja.client.core
  (:use [calaja.client.game]
        [calaja.client.render]
        [clojure.set])
  (:import [java.awt RenderingHints]
           [java.awt.event KeyListener KeyEvent]
           [javax.swing JFrame SwingUtilities JPanel]))


(def rendering-hints (RenderingHints.
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON))

(def key-actions {:one {KeyEvent/VK_UP    :thrust
                        KeyEvent/VK_LEFT  :left
                        KeyEvent/VK_RIGHT :right
                        KeyEvent/VK_DOWN  :shoot}
                  :two {KeyEvent/VK_W     :thrust
                        KeyEvent/VK_A     :left
                        KeyEvent/VK_D     :right
                        KeyEvent/VK_S     :shoot}})


(def game-delay (atom 20))
(def keys-held (atom #{}))

(def game (new-game [800 800]))


(defn now []
  (System/currentTimeMillis))


(defn get-actions [player key-events]
  (let [events (-> player :name key-actions)]
    (->> events keys set (intersection key-events) (map events))))


(defn get-delay-fn [keys]
  (cond
    (keys KeyEvent/VK_EQUALS) dec
    (keys KeyEvent/VK_MINUS)  inc
    :else                     identity))


(defn render [g]
  (let [sprites (concat @(:players game) @(:bullets game))]
    (doseq [sprite sprites]
      (draw sprite g))))


(defn step [dt]
  (let [players   @(:players game)
        actions   (map #(get-actions % @keys-held) players)
        delay-fn  (get-delay-fn @keys-held)]
    (swap! game-delay delay-fn)
    (step-game game actions dt)))


(defn new-canvas []

  (proxy [JPanel KeyListener Runnable] []

    (paint [g]
      (proxy-super paint g)
      (.setRenderingHints g rendering-hints)
      (render g))

    (keyTyped [_])

    (keyReleased [ev]
      (swap! keys-held disj (.getKeyCode ev)))

    (keyPressed [ev]
      (swap! keys-held conj (.getKeyCode ev)))

    (addNotify []
      (proxy-super addNotify)
      (.start (Thread. this)))

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
  (let [frame (JFrame. "Calaja")
        canvas (new-canvas)
        [width height] (:bounds game)]
    (doto canvas
      (.setDoubleBuffered true)
      (.setFocusable true)
      (.addKeyListener canvas)
      (.setSize width height)
      (.setVisible true))
    (doto frame
      (.setSize width height)
      (.setContentPane canvas)
      (.setVisible true))))


(defn -main [& args]
  (SwingUtilities/invokeLater start-game))

(-main)