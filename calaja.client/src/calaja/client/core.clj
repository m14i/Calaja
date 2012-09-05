(ns calaja.client.core
  (:use [calaja.client.game]
        [calaja.client.render]
        [clojure.set])
  (:import [java.awt RenderingHints Dimension Frame Toolkit Graphics2D]
           [java.awt.event KeyListener KeyEvent]
           [javax.swing JFrame SwingUtilities JPanel]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(def rendering-hints (RenderingHints. RenderingHints/KEY_ANTIALIASING
                                      RenderingHints/VALUE_ANTIALIAS_ON))

(def key-actions {:one {KeyEvent/VK_UP    :thrust
                        KeyEvent/VK_LEFT  :left
                        KeyEvent/VK_RIGHT :right
                        KeyEvent/VK_DOWN  :shoot}
                  :two {KeyEvent/VK_W     :thrust
                        KeyEvent/VK_A     :left
                        KeyEvent/VK_D     :right
                        KeyEvent/VK_S     :shoot}})


(def game-delay (atom 17))
(def keys-held (atom #{}))


(def game
  (let [screen  (-> (Toolkit/getDefaultToolkit) .getScreenSize)
        width   (-> screen .getWidth int)
        height  (-> screen .getHeight int)]
    (new-game [width height])))


(defn now []
  (System/currentTimeMillis))


(defn get-actions [player key-events]
  (let [events (-> player :name key-actions)]
    (->> events keys set (intersection key-events) (map events))))


(defn get-delay-fn [keys]
  (cond (keys KeyEvent/VK_EQUALS) dec
        (keys KeyEvent/VK_MINUS)  inc
        :else                     identity))


(defn step [dt]
  (let [players   (:players game)
        actions   (map #(get-actions % @keys-held) @players)
        delay-fn  (get-delay-fn @keys-held)]
    (swap! game-delay delay-fn)
    (step-game game actions dt)))


(defn ^JPanel new-canvas []

  (proxy [JPanel KeyListener Runnable] []

    (paint [g]
      (let [^JPanel this this] (proxy-super paint g))
      (.setRenderingHints ^Graphics2D g rendering-hints)
      (draw game g))

    (keyTyped [_])

    (keyReleased [ev]
      (swap! keys-held disj (.getKeyCode ^KeyEvent ev)))

    (keyPressed [ev]
      (swap! keys-held conj (.getKeyCode ^KeyEvent ev)))

    (addNotify []
      (let [^JPanel this this] (proxy-super addNotify))
      (.start (Thread. ^Runnable this)))

    (run []
      (loop [ti (now)]
        (let [tj (now)
              dt (- tj ti)]
          (step dt)
          (.repaint ^JPanel this)
          (let [elapsed (- (now) tj)
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
      (.setPreferredSize (Dimension. width height))
      (.setVisible true))
    (doto frame
      (.setUndecorated true)
      (.setExtendedState Frame/MAXIMIZED_BOTH)
      (.setContentPane canvas)
      (.pack)
      (.setVisible true))))


(defn -main [& args]
  (SwingUtilities/invokeLater start-game))