(ns ^:shared schelling-app.model)

(defn vacancies [{:keys [width height population]}]
  (- (* width height) population))

(defn setup [{:keys [population threshold]}]
  {:pre [(<= population (* 50 50))]}
  (let [n (/ population 2)
        w 50
        h 50
        state {:population population
               :threshold threshold
               :width w
               :height h}]
    (assoc state :neighborhood ;(zipmap (for [x (range w)
                               ;              y (range h)]
                               ;          [x y])
                                       (shuffle
                                         (concat (repeat n 0)
                                                 (repeat (- population n) 1)
                                                 (repeat (vacancies state) nil))))))

(defn coord->idx [w h [x y]]
  (let [x (mod x w)
        y (mod y h)]
    (+ (* y w) x)))

(defn find-happiness-from-values [threshold subject & neighbors]
  (if (nil? subject)
    :vacant
    (let [present-neighbors (remove nil? neighbors)
          similar (count (filter (partial = subject) neighbors))
          different (- (count neighbors) similar)]
      (if (<= threshold (/ similar different))
        :happy
        :unhappy))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ x dx) (+ y dy)]))
; (def neighbors (memoize neighbors))

(defn find-happiness [{:keys [neighborhood threshold width height]} pos]
  (apply find-happiness-from-values
         (/ threshold 100)
         (mapv (comp neighborhood (partial coord->idx width height))
                     (list* pos (neighbors pos)))))

(defn step [{:keys [neighborhood threshold width height] :as state}]
  (let [positions (for [x (range width) y (range height)] [x y])
        {:keys [happy unhappy vacant] :as states} (group-by (partial find-happiness state)
                                                 positions)
        unhappy-idxs (map (partial coord->idx width height) unhappy)
        vacant-idxs (map (partial coord->idx width height) vacant)
        ; unhappy-or-vacant (concat unhapy-idxs vacant-idxs)
        destinations (shuffle (concat unhappy-idxs vacant-idxs))
        updates (interleave destinations
                            (concat (map neighborhood unhappy-idxs)
                                    (repeat (count vacant) nil)))
        updates (concat
                  (interleave unhappy-idxs
                          (repeat nil))
                  (interleave (take (count unhappy-idxs) destinations)
                          (map neighborhood unhappy-idxs)))
        new-neighborhood (if (empty? updates)
                           neighborhood
                           (apply assoc neighborhood updates))]
    ; (.log js/console (pr-str (find-happiness state [0 0])))
    ; (.log js/console (pr-str destinations))
    (.log js/console (pr-str (map #(update-in % [1] count) states)))
    ; (.log js/console (pr-str updates))
    ; (.log js/console (pr-str [(count destinations) (count unhappy-idxs) (count vacant)]))
    (assoc state :neighborhood new-neighborhood)))

