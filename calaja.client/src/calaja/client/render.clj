(ns calaja.client.render

  (:use [calaja.client.game]
        [calaja.client.tools])

  ;; using records needs require AND import
  (:require [calaja.client.game])
  (:import [calaja.client.game Element Player]))


(defprotocol ISprite
  (draw [this g]))


(extend-type Element
  ISprite
  (draw [this g]
    (.draw g (:tshape this))))


(extend-type Player
  ISprite
  (draw [this g]
    (draw (:element this) g)))
