(ns calaja.client.game
  (:use [calaja.client.functions]
        [calaja.client.model]
        [clojure.pprint])
  (:require [calaja.client.model])
  (:import [calaja.client.model Element Player Bullet Game]))


;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ship-shoot-time  200)
(def ship-thrust      0.0005)
(def ship-spinr       0.01)
(def ship-spinl       (- ship-spinr))
(def ship-energy      20)


;; constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (Bullet. 1 1000 (Element. [x y] 0 v 0 0 circle circle))))


(defn new-player [name energy point]
  (let [shape (new-ship 10)]
    (Player. name energy 0 (Element. point 0 [0 0] 0 0 shape shape))))


(defn new-game [bounds]
  (let [players [(new-player :one ship-energy (mapv #(/ % 2) bounds))
                 (new-player :two ship-energy (mapv #(-> % (/ 2) (+ 100)) bounds))]]
    (Game. bounds (ref players) (ref []))))


;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprotocol IMove
  (move [this limits dt]))


(defprotocol IActor
  (act [this action])
  (shooting? [this]))


(defprotocol IAge
  (age [this dt])
  (alive? [this]))


;; extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (update-in this [:element ] move limits dt))

  IAge

  (age [this dt]
    (update-in this [:alive ] - dt))

  (alive? [this]
    (and
      (< 0 (:alive this))
      (< 0 (:energy this)))))


(extend-type Player

  IMove

  (move [this limits dt]
    (update-in this [:element ] move limits dt))

  IActor

  (act [this actions]
    (let [reset-player (-> this
                          (assoc-in [:element :spin ] 0)
                          (assoc-in [:element :thrust ] 0))]
      (reduce
        (fn [player action]
          (case action
            :shoot  (update-in  player [:shootDelay ]       #(if (zero? %) ship-shoot-time %))
            :thrust (assoc-in   player [:element :thrust ]  ship-thrust)
            :right  (assoc-in   player [:element :spin ]    ship-spinr)
            :left   (assoc-in   player [:element :spin ]    ship-spinl)
            player))
        reset-player
        actions)))

  (shooting? [this]
    (= ship-shoot-time (:shootDelay this)))

  IAge

  (age [this dt]
    (update-in this [:shootDelay] #(max 0 (- % dt))))

  (alive? [this]
    (< 0 (:energy this))))



;; game logic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn interact [player bullet]
  (let [pbox  (get-bbox player)
        bbox  (get-bbox bullet)
        dfn   (if (.intersects pbox bbox) dec identity)]
    [(update-in player [:energy ] dfn)
     (update-in bullet [:energy ] dfn)]))


(defn hit-player [player bullets]
  (if (empty? bullets)
    [player bullets]
    (let [result          (map interact (repeat player) bullets)
          updated-player  (apply min-key :energy (map first result))
          updated-bullets (mapv second result)]
      [updated-player updated-bullets])))


(defn step-interactions [players bullets]
  (let [result          (map #(hit-player % bullets) players)
        updated-players (map first result)
        updated-bullets (map #(apply min-key :energy %) (apply map vector (map second result)))]
    [updated-players updated-bullets]))


(defn step-players [players bullets actions bounds dt]
  (let [alive-players (filter alive? players)]
    (map
      (fn [player action]
        (-> player
          (age dt)
          (act action)
          (move bounds dt)))
      alive-players
      actions)))


(defn step-bullets [bullets players bounds dt]
  (vec (concat
         (->> bullets
           (filter  alive?)
           (map     #(-> %
                       (age dt)
                       (move bounds dt))))
         (->> players
           (filter  shooting?)
           (map     new-bullet)))))


;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn step-game [game actions dt]
  (let [{:keys [bounds players bullets]}  game
        moved-players                     (step-players @players @bullets actions bounds dt)
        moved-bullets                     (step-bullets @bullets @players bounds dt)
        [updated-players updated-bullets] (step-interactions moved-players moved-bullets)]

    (dosync
      (ref-set players updated-players)
      (ref-set bullets updated-bullets))))