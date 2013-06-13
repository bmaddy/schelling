(ns ^:shared schelling-app.model)

(defn vacancies [{:keys [width height population]}]
  (- (* width height) population))

(defn coord->idx [w h [x y]]
  (let [x (mod x w)
        y (mod y h)]
    (+ (* y w) x)))

(defn idx->coord [w h idx]
  (let [x (rem idx w)
        y (quot idx w)]
    [x y]))

(defn vacant-happy-or-unhappy? [threshold subject & neighbors]
  (if (nil? subject)
    :vacant
    (let [present-neighbors (remove nil? neighbors)
          similar (count (filter (partial = subject) neighbors))
          different (- (count neighbors) similar)]
      (if (<= threshold (/ similar different))
        :happy
        :unhappy))))

(defn neighbors [width height idx]
  (let [[x y] (idx->coord width height idx)]
    (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
      (coord->idx width height [(+ x dx) (+ y dy)]))))
(def neighbors (memoize neighbors))

(defn find-happiness [{:keys [neighborhood threshold width height]} idx]
  (apply vacant-happy-or-unhappy?
         (/ threshold 100)
         (mapv neighborhood (list* idx (neighbors width height idx)))))

(defn find-unhappy-and-vacant-idxs [{:keys [width height] :as state}]
  (if (and (:unhappy state) (:vacant state))
    state
    (let [indexes (range (* width height))
          {:keys [unhappy vacant] :as states} (group-by (partial find-happiness state) indexes)]
      (.log js/console (pr-str (map #(update-in % [1] count) states)))
      {:unhappy unhappy
       :vacant vacant})))

(defn setup [{:keys [population threshold]}]
  {:pre [(<= population (* 50 50))]}
  (let [n (/ population 2)
        w 50
        h 50
        state {:population population
               :threshold threshold
               :width w
               :height h}
        state (assoc state :neighborhood (shuffle
                                           (concat (repeat n 0)
                                                   (repeat (- population n) 1)
                                                   (repeat (vacancies state) nil))))]
    (merge state (find-unhappy-and-vacant-idxs state))))

(defn step [{:keys [neighborhood threshold width height] :as state}]
  (.profile js/console "step")
  (let [{:keys [unhappy vacant]} (find-unhappy-and-vacant-idxs state)
        destinations (shuffle (concat unhappy vacant))
        updates (concat
                  (interleave unhappy
                          (repeat nil))
                  (interleave (take (count unhappy) destinations)
                          (map neighborhood unhappy)))
        new-neighborhood (if (empty? updates)
                           neighborhood
                           (apply assoc neighborhood updates))
        new-state (assoc state :neighborhood new-neighborhood :unhappy nil :vacant nil)
        new-state (merge new-state (find-unhappy-and-vacant-idxs new-state))]
    (.profileEnd js/console)
    new-state))

