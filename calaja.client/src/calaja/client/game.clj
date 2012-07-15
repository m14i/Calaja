(ns calaja.client.game
  (:use [calaja.client.functions]
        [calaja.client.model])
  (:require [calaja.client.model])
  (:import [calaja.client.model Element Player Bullet Game]))


(defprotocol IMove
  (move [this limits dt]))

(defprotocol IShoot
  (shoot [this]))


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
    (update-in this [:element ] move limits dt)))


(extend-type Player
  IMove
  (move [this limits dt]
    (update-in this [:element ] move limits dt))

  IShoot
  (shoot [this]
    (new-bullet this)))


(defn new-game [bounds]
  (let [bullets (atom [])
        players (atom [(new-player :one 1 (mapv #(/ % 2) bounds))
                       (new-player :two 1 (mapv #(-> % (/ 2) (+ 100)) bounds))])]
    (Game. bounds players bullets)))

(defn process-hit [player bullets]
  (let [pbox (get-bbox player)
        bboxes (map get-bbox bullets)]
    (if (some #(.intersects pbox %) bboxes)
      (update-in player [:energy ] dec)
      player)))


(defn step-players [players bullets actions bounds dt]
  (map #(let [[p a] %]
          (-> p
            (update-player a)
            (move bounds dt)
            ;(process-hit bullets)
            ))
    (map vector players actions)))


(defn step-bullets [bullets players bounds dt]
  (vec (concat
         (->> bullets
           (filter #(< 0 (:alive %)))
           (filter #(< 0 (:energy %)))
           (map #(move (update-in % [:alive ] - dt) bounds dt)))
         (->> players
           (filter :shoot )
           (map shoot)))))


(defn step-game [game actions dt]
  (let [{:keys [bounds players bullets]} game]
    (swap! players step-players @bullets actions bounds dt)
    (swap! bullets step-bullets @players bounds dt)))


;; move players
;; move bullets
;; check interactions


(defn interact [player bullet]
  (if (and
        (< 0 (:alive bullet))
        (< 0 (:energy bullet)))
    (let [pbox (get-bbox player)
          bbox (get-bbox bullet)]
      (if (.intersects pbox bbox)
        [(update-in player [:energy ] dec)
         (update-in bullet [:energy ] dec)]
        [player bullet]))
    [player bullet]))


(defn apply-in [fn ks & m]
  (reduce fn (map #(get-in % ks) m)))


(defn bla [player bullets]
  (let [res (map interact (repeat player) bullets)]
    [(min-key :energy (map first res)) (mapv second res)]))


(defn step-interactions [players bullets]
  (let [res (mapcat #(bla % bullets) players)
        bs (map rest res)]
    [(map first res) (map #(apply min-key :energy %) (apply map vector bs))]))








