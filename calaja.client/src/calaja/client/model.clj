(ns calaja.client.model)

(defrecord Element  [point ^double angle velocity ^double thrust ^double spin shape tshape])
(defrecord Player   [name ^int energy shootDelay element])
(defrecord Bullet   [^int energy ^int alive element])
(defrecord Game     [bounds players bullets])