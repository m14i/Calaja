(ns calaja.client.model)

(defrecord Element  [point angle velocity thrust spin shape tshape])
(defrecord Player   [name energy shoot actions element])
(defrecord Bullet   [player energy alive element])
(defrecord Game     [bounds players bullets])
