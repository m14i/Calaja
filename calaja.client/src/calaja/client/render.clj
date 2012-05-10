(ns calaja.client.render
  (:use calaja.client.game))

(defn transform [sprite]
  (let [[x y] (:point sprite)
        angle (:angle sprite)
        shape (:shape sprite)]
    (-> (doto (AffineTransform.)
          (.translate x y)
          (.rotate angle))
      (.createTransformedShape shape))))

(defprotocol ISprite
  (draw [this g]))

(extend Element
  ISprite
  (draw [this g]
    (.draw g (transform this))))

(extend Player
  ISprite
  (draw [this g]
    (draw (:element this) g)))


