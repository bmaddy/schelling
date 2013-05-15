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
    (assoc state :neighborhood (zipmap (for [x (range w)
                                             y (range h)]
                                         [x y])
                                       (shuffle
                                         (concat (repeat n 0)
                                                 (repeat (- population n) 1)
                                                 (repeat (vacancies state) nil)))))))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ x dx) (+ y dy)]))

(defn step [{:keys [neighborhood threshold] :as state}]
  (assoc state :neighborhood (move-unhappy-neighbors neighborhood (* 8 (/ threshold 100)))))

(defn similar-neighbors [neighborhood pos]
  (count (filter (partial = (neighborhood pos)) (map neighborhood (neighbors pos)))))

(defn move-unhappy-neighbors [neighborhood neighbors-wanted]
  (.log js/console (str "neighbors-wanted: " neighbors-wanted))
  (loop [neighborhood neighborhood
         locs (keys (filter #(second %) neighborhood))
         moved 0
         vacancies (set (keys (filter #(nil? (second %)) neighborhood)))]
    (cond
      (empty? locs) (do
                      (.log js/console (str "moved: " moved))
                      neighborhood)
      (<= neighbors-wanted (similar-neighbors neighborhood (first locs))) (do
                                                                            ; (.log js/console "skip")
                                                                            (recur neighborhood
                                                                                 (rest locs)
                                                                                 moved
                                                                                 vacancies))
      :else (let [src (first locs)
                  dest (rand-nth vacancies)]
              ; (.log js/console (str "similar-neighbors: " (similar-neighbors neighborhood src)))
              ; (.log js/console (str "moving " (pr-str src) " to " (pr-str dest)))
              (recur (assoc neighborhood src nil dest (neighborhood src))
                     (rest locs)
                     (inc moved)
                     (-> vacancies
                         (disj dest)
                         (conj src)))))))

