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
    (let [{:keys [shape point angle]} this
          transformed-shape (-> this :shape )]
      (splat nil "before draw: " transformed-shape)
      (.draw g transformed-shape))))


(extend-type Player
  ISprite
  (draw [this g]
    (update-in this [:element ] draw g)))
