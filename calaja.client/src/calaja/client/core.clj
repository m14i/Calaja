(ns calaja.client.core
   (:import (javax.swing JFrame JPanel)
            (java.awt RenderingHints Color BasicStroke)
            (java.awt.geom Ellipse2D AffineTransform Path2D)
            (java.awt.event KeyListener KeyEvent)))

(def height 800)
(def width 800)
(def throttle 20)
(def keys-held (atom #{}))
(def unit (/ 1 (Math/sqrt 2)))

(def ship-path {:x (for [x [0 3 2 1 0 -1 -2 -3 0]] (* 20 x unit))
                :y (for [y [3 0 -1 0 -1 0 -1 0 3]] (* 20 y unit))})

(defn mk-actor [path name lives shield loc rot vel]
   {:name name
    :path path
    :lives (atom lives)
    :shield (atom shield)
    :loc (atom loc)
    :rot (atom rot)
    :vel (atom vel)})

(def player (mk-actor ship-path "player" 3 3 [0 0] 0 [0 0]))

(def actors
   (atom [(mk-actor ship-path "ship" 3 3 [50 50] 0 [0 0])]))

(def hints
   (RenderingHints.
      RenderingHints/KEY_ANTIALIASING
      RenderingHints/VALUE_ANTIALIAS_ON))

(defn limits [canvas]
   {:x (-> canvas .getSize .getWidth (/ 2))
    :y (-> canvas .getSize .getHeight (/ 2))})

(defn mk-path [{:keys [x y]}]
   (let [path (java.awt.geom.Path2D$Double.)
         points (map vector x y)]
      (do
         (.moveTo path (first x) (first y))
         (doseq [[xp yp] points] (.lineTo path xp yp))
         path)))

(defn transform [shape loc rot]
   (let [[x y] loc
         t (doto (AffineTransform.) (.translate x y) (.rotate rot))]
      (.createTransformedShape t shape)))

(defn draw-actor [g actor]
   (let [path (mk-path (:path actor))
         loc (:loc actor)
         rot (:rot actor)
         sprite (transform path @loc @rot)]
      (.draw g sprite)))

(defn mk-origin-transfom [canvas]
   (let [{:keys [x y]} (limits canvas)]
      (doto (AffineTransform.)
         (.translate x y)
         (.scale 1 -1))))

(defn draw [g]
   (doseq [a (conj @actors player)]
      (draw-actor g a)))

(defn accelerate [current rot value]
   (let [[x y] current
         dx (-> rot Math/sin - (* value))
         dy (-> rot Math/cos (* value))]
      [(+ x dx) (+ y dy)]))

(defn rotate-by []
   (cond
      (@keys-held KeyEvent/VK_LEFT) 0.1
      (@keys-held KeyEvent/VK_RIGHT) -0.1
      :else 0))

(defn accelerate-by []
   (cond
      (@keys-held KeyEvent/VK_UP) 0.0005
      (@keys-held KeyEvent/VK_DOWN) -0.0005
      :else 0))

(defn process-keys [player]
   (let [r (:rot player)]
      (do
         (swap! r #(+ % (rotate-by)))
         (swap! (:vel player) #(accelerate % @r (accelerate-by))))))

(defn move [player dt]
   (let [v (:vel player)
         ds (map (partial * dt) @v)]
      (swap! (:loc player) #(map + ds %))))

(defn step [dt]
   (do
      (process-keys player)
      (move player dt)))

(defn mk-canvas []
   (proxy [JPanel KeyListener Runnable] []
      (paintComponent [g]
         (proxy-super paintComponent g)
         (do
            (.setTransform g (mk-origin-transfom this))
            (.setRenderingHints g hints)
            (draw g)))
      (keyTyped [_])
      (keyReleased [ev]
         (swap! keys-held #(disj % (.getKeyCode ev))))
      (keyPressed [ev]
         (swap! keys-held #(conj % (.getKeyCode ev))))
      (addNotify []
         (proxy-super addNotify)
         (-> (Thread. this) .start))
      (run []
         (loop [t0 (System/currentTimeMillis)]
            (let [tn (System/currentTimeMillis)
                  dt (- tn t0)]
               (do
                  (println (:vel player))
                  (step dt)
                  (.repaint this)
                  (let [elapsed (- (System/currentTimeMillis) t0)
                        sleep (max 2 (- throttle elapsed))]
                     (Thread/sleep sleep)
                     (recur tn))))))))

(defn show []
   (let [canvas (mk-canvas)
         frame (JFrame. "Calaja")]
      (doto canvas
         (.setFocusable true)
         (.addKeyListener canvas))
      (doto frame
         (.setContentPane canvas)
         (.setSize width height)
         (.setVisible true))))

(defn -main [& args]
   (show))


