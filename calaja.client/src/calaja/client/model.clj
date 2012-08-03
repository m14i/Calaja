(ns calaja.client.model)

(defrecord Element  [point angle velocity thrust spin shape tshape])
(defrecord Player   [name energy shootDelay element])
(defrecord Bullet   [energy alive element])
(defrecord Game     [bounds players bullets])
