(ns snake-clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(def field-width 10)
(def field-height 10)
(def tick-ms 200)
(def win-size (- (* field-width field-height) 5))
(def scale 20)

(defn create-snake []
  {:body [[3 1] [2 1]]
   :heading [1 0]})

(defn create-apple []
  {:position [(rand-int field-width) (rand-int field-height)]})

(defn create-apple-away-from-snake [{body :body}] 
  (first (remove #(contains? (set body) (:position %)) (repeatedly create-apple))))

(defn move-forward [head heading]
  (map + head heading))

(defn move [snake grow]
  (let [body (:body snake) heading (:heading snake)] 
    (assoc snake 
           :body (conj (if grow body (butlast body)) (move-forward (first body) heading)))))

(defn turn [{:keys [body heading] :as snake} new-heading]
  (if (= [0 0] (map * heading new-heading)) 
    (assoc snake :heading new-heading)
    snake))

(defn eat? [snake apple]
  (let [head (first (:body snake))]
  (= head (:position apple))))

(defn win? [{body :body}] 
  (>= (count body) win-size))

(defn hit-body? [{body :body }]
  (let [[head & tail] body]
    (contains? (set tail) head)))

(defn hit-wall? [snake]
  (let [head (first (:body snake))]
    (cond
      (neg? (first head)) true
      (neg? (second head)) true
      (<= field-width (first head)) true
      (<= field-height (second head)) true
      :else false)))

(defn lose? [snake]
  (or (hit-body? snake) (hit-wall? snake)))

(defn update-step [{:keys  [snake apple] :as state}]
  (if (eat? snake apple)
    (let [new-snake (move snake true)] 
      (assoc state :snake new-snake :apple (create-apple-away-from-snake new-snake)))
    (assoc state :snake (move snake false))))

(defn time-to-move? [timer]
  (< tick-ms (- (q/millis) timer)))

(defn reset-game [] 
  {:snake (create-snake) 
   :apple (create-apple) 
   :timer 0})

(defn update-direction [snake]
  (if (q/key-pressed?)
    (cond
      (= (q/key-as-keyword) :up) (turn snake [0 -1])
      (= (q/key-as-keyword) :down)(turn snake [0 1])
      (= (q/key-as-keyword) :left) (turn snake [-1 0])
      (= (q/key-as-keyword) :right) (turn snake [1 0])
      :else snake)
    snake))

(defn setup []
  (q/frame-rate 60)
  (reset-game))

(defn update-state [{:keys [snake apple timer] :as state}]
  (if (time-to-move? timer)
    (let [{new-snake :snake :as new-state} (update-step state)] 
      (cond 
        (win? new-snake) (reset-game)
        (lose? new-snake) (reset-game)
        :else (assoc new-state :timer (q/millis))))
    (-> state
        (update-in [:snake] update-direction)
        )))

(defn map-point-to-rect [x y]
  [(* scale x) (* scale y) scale scale])

(defn draw-point [x y]
  (apply q/rect (map-point-to-rect x y)))

(defn draw-apple [{position :position}]
  (q/fill 255 0 0)
  (apply draw-point position))

(defn draw-snake [{body :body}]
  (q/fill 0 0 255)
  (q/stroke 0 0 0)
  (dorun (map (partial apply draw-point) body )))

(defn draw-field [width height]
  (q/fill 0 255 0)
  (q/no-stroke)
  (q/rect 0 0 (* scale field-width) (* scale field-height)))

(defn draw-state [{:keys [snake apple] :as state}]
  (q/background 255)
  (draw-field field-width field-height)
  (draw-apple apple)
  (draw-snake snake))

(defn -main []
  (q/defsketch snake-clj
    :size (map (partial * scale) [field-width field-height])
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
