(ns calaja.client.game
  (:use [calaja.client.functions]
        [calaja.client.model]
        [clojure.pprint])
  (:require [calaja.client.model])
  (:import [calaja.client.model
            Element Player Bullet Game
            PolarCoordinate CartesianCoordinate]))


;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:const ship-shoot-time  200)
(def ^:const ship-thrust      0.0001)
(def ^:const ship-spinr       0.003)
(def ^:const ship-spinl       (- ship-spinr))
(def ^:const ship-energy      20)
(def ^:const ship-size        10)
(def ^:const ship-max-speed   0.4)

(def ^:const bullet-radius 5)
(def ^:const bullet-speed  (+ ship-max-speed 0.3))

;; constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn new-ship [size]
  (letfn [(scale [x] (->> 2 Math/sqrt (/ 1) (* size x)))]
    (new-path (map scale [0  3  2  1  0 -1 -2 -3])
              (map scale [2 -1 -2 -1 -2 -1 -2 -1]))))


(defn new-bullet [player]
  (let [{:keys [angle point]} (:element player)
        ds     (PolarCoordinate. bullet-speed angle)
        offset (+ ship-size (* 3 bullet-radius))
        source (sum point (PolarCoordinate. offset angle))
        circle (java.awt.geom.Ellipse2D$Float. 0 0 bullet-radius bullet-radius)]
    (Bullet. 1 1000 (Element. source 0 ds 0 0 circle circle))))


(defn new-player [name energy x y]
  (let [ds (CartesianCoordinate. 0 0)
        point (CartesianCoordinate. x y)
        shape (new-ship ship-size)]
    (Player. name energy 0 (Element. point 0 ds 0 0 shape shape))))


(defn new-game [bounds]
  (let [[x y] (mapv #(/ % 2) bounds)
        players [(new-player :one ship-energy x y)
                 (new-player :two ship-energy (+ x 100) (+ y 100))]]
    (Game. bounds (atom players) (atom []))))


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
    (update-in this [:element] move limits dt))

  IAge

  (age [this dt]
    (update-in this [:alive] - dt))

  (alive? [this]
    (and (< 0 (:alive this))
         (< 0 (:energy this)))))


(extend-type Player

  IMove

  (move [this limits dt]
    (update-in this [:element ] move limits dt))

  IActor

  (act [this actions]
    (let [reset-player (-> this
                           (assoc-in [:element :spin] 0)
                           (assoc-in [:element :thrust] 0))]
      (reduce (fn [player action]
                (case action
                  :shoot  (update-in  player [:shootDelay]       #(if (zero? %) ship-shoot-time %))
                  :thrust (assoc-in   player [:element :thrust]  ship-thrust)
                  :right  (assoc-in   player [:element :spin]    ship-spinr)
                  :left   (assoc-in   player [:element :spin]    ship-spinl)
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
        bbox  (get-bbox bullet)]
    (if (.intersects pbox bbox) 1 0)))


(defn hit-player [player bullets]
  (let [result          (map interact (repeat player) bullets)
        lost-energy     (apply + result)
        updated-player  (update-in player [:energy] - lost-energy)
        updated-bullets (map #(update-in %1 [:energy] - %2) bullets result)]
    [updated-player updated-bullets]))


(defn step-interactions [players bullets]  
  (let [result          (map #(hit-player % bullets) players)
        updated-players (map first result)
        updated-bullets (map #(apply min-key :energy %) (apply map vector (map second result)))]
    [updated-players updated-bullets]))


(defn cap-velocity [player]
  (update-in player [:element :velocity] #(let [{:keys [radius theta]} (polar %)
                                                r (min radius ship-max-speed)]
                                            (PolarCoordinate. r theta))))


(defn step-players [players bullets actions bounds dt]
  (let [alive-players (filter alive? players)]
    (map (fn [player action]
           (-> player
               (age dt)
               (act action)
               (move bounds dt)
               (cap-velocity)))
         alive-players
         actions)))


(defn step-bullets [bullets players bounds dt]
  (let [alive-bullets (filter alive? bullets)
        new-bullets (->> players (filter shooting?) (map new-bullet))
        all-bullets (concat alive-bullets new-bullets)]
    (map #(-> % (age dt) (move bounds dt)) all-bullets)))
          


;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn step-game [game actions dt]
  (let [{:keys [bounds players bullets]}  game
        moved-bullets (future (step-bullets @bullets @players bounds dt))
        moved-players (future (step-players @players @bullets actions bounds dt))
        [next-players next-bullets] (step-interactions @moved-players @moved-bullets)]
    
    (reset! players next-players)
    (reset! bullets next-bullets)))