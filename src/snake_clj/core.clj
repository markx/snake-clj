(ns snake-clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30))

(defn draw [])

(q/defsketch snake-clj
  :size [500 500]
  :setup setup
  :draw draw
  :features [:keep-on-top])
