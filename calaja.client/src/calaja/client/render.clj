(ns calaja.client.render

  (:use [calaja.client.game]
        [calaja.client.tools])

  ;; using records needs require AND import
  (:require [calaja.client.game])
  (:import [calaja.client.game Element Player]
           [java.awt Font]))


(defprotocol ISprite
  (draw [this g]))


(extend-type Element
  ISprite
  (draw [this g]
    (.draw g (:tshape this))))


(extend-type Player
  ISprite
  (draw [this g]
    (let [element (:element this)
          [x y] (mapv int (:point element))
          energy (:energy this)
          xt (+ x 20)
          yt (- y 30)]

      (draw element g)

      (.setFont g (Font. Font/SANS_SERIF Font/BOLD 10))
      (.drawString g (name (:name this)) xt yt)
      (.drawString g (str energy) xt (+ yt 10)))))
