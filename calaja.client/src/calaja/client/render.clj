(ns calaja.client.render
  (:use [calaja.client.functions])
  (:require [calaja.client.model])
  (:import [calaja.client.model Game Element Player Bullet]
           [java.awt Font]))


(defprotocol ISprite
  (draw [this g]))


(extend-type Element
  ISprite
  (draw [this g]
    (.draw g (:tshape this))))


(extend-type Bullet
  ISprite
  (draw [this g]
    (draw (:element this) g)))


(extend-type Player
  ISprite
  (draw [this g]
    (let [element (:element this)
          [x y]   (mapv int (:point element))
          energy  (:energy this)
          xt      (+ x 20)
          yt      (- y 30)]

      (draw element g)

      (.setFont     g  (Font. Font/SANS_SERIF Font/BOLD 10))
      (.drawString  g  (name (:name this)) xt yt)
      (.drawString  g  (str energy) xt (+ yt 10)))))


(extend-type Game
  ISprite
  (draw [this g]
    (let [sprites (concat @(:players this) @(:bullets this))]
      (doseq [sprite sprites]
        (draw sprite g)))))
