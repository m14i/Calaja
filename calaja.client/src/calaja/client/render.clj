(ns calaja.client.render

  (:use [calaja.client.game])

  ;; using records needs require AND import
  (:require [calaja.client.game])
  (:import [calaja.client.game Element Player]))


(defprotocol ISprite
  (draw [this g]))


(extend-type Element
  ISprite
  (draw [this g]
    (let [{:keys [shape point angle]} this]
      (.draw g (transform shape point angle)))))


(extend-type Player
  ISprite
  (draw [this g]
    (update-in this [:element ] draw g)))
