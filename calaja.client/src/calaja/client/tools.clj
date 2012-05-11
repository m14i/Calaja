(ns calaja.client.tools)

(defn splat [x & xs]
  (do
    (println xs)
    x))


(defn now []
  (System/currentTimeMillis))