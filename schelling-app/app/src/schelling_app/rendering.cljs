(ns schelling-app.rendering
  (:require [domina :as dom]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.protocols :as p]
            [io.pedestal.app.render.events :as events]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render.push.handlers.automatic :as d])
  (:require-macros [schelling-app.html-templates :as html-templates]))

;; Load templates.

(def templates (html-templates/schelling-app-templates))

;; The way rendering is handled below is the result of using the
;; renderer provided in `io.pedestal.app.render`. The only requirement
;; for a renderer is that it must implement the Renderer protocol.
;;
;; This renderer dispatches to rendering functions based on the
;; requested change. See the render-config table below. Each render
;; function takes three arguments: renderer, render operation and a
;; a transmitter which is used to send data back to the application's
;; behavior. This example does not use the transmitter.

(defn render-page [renderer [_ path] transmitter]
  (let [;; The renderer that we are using here helps us map changes to
        ;; the UI tree to the DOM. It keeps a mapping of paths to DOM
        ;; ids. The `get-parent-id` function will return the DOM id of
        ;; the parent of the node at path. If the path is [:a :b :c]
        ;; then this will find the id associated with [:a :b]. The
        ;; root node [] is configured when we created the renderer.
        parent (render/get-parent-id renderer path)
        ;; Use the `new-id!` function to associate a new id to the
        ;; given path. With two arguments, this function will generate
        ;; a random unique id. With three arguments, the given id will
        ;; be associated with the given path.
        id (render/new-id! renderer path)
        ;; Get the dynamic template named :schelling-app-page
        ;; from the templates map. The `add-template` function will
        ;; associate this template with the node at
        ;; path. `add-template` returns a function that can be called
        ;; to generate the initial HTML.
        html (templates/add-template renderer path (:schelling-app-page templates))]
    ;; Call the `html` function, passing the initial values for the
    ;; template. This returns an HTML string which is then added to
    ;; the DOM using Domina.
    (dom/append! (dom/by-id parent) (html {:id id :message "" :count "from render-page"}))

    ; setup button
    (events/send-on :click
                    (dom/by-id "setup")
                    transmitter
                    (fn []
                      (let [population (aget (dom/by-id "population") "value")
                            threshold (aget (dom/by-id "threshold") "value")]
                             (msg/fill :schelling-state
                                       [{msg/topic :schelling-state
                                         msg/type :setup
                                         :value {:population (js/parseInt population) :threshold (js/parseInt threshold)}}]))))

    ;step button
    (events/send-on-click (dom/by-id "step")
                          transmitter
                          :schelling-state
                          [{msg/topic :schelling-state msg/type :step}])

    ;run button
    (events/send-on-click (dom/by-id "run")
                          transmitter
                          :schelling-state
                          [{msg/topic :running? msg/type :toggle}])

    (events/send-on-click (dom/by-id "add-counter")
                          transmitter
                          :example-transform
                          [{msg/topic :example-transform msg/type :change :value "foo"}])

    ; enable the setup buttons
    (dom/remove-class! (dom/by-id "setup") "disabled")))

(defn render-message [renderer [_ path _ new-value] transmitter]
  ;; This function responds to a :value event. It uses the
  ;; `update-t` function to update the template at `path` with the new
  ;; values in the passed map.
  (templates/update-t renderer path {:message new-value}))

(defn rgb-str [a]
  (str "rgb("
       (if (nil? a)
         "255,255,255"
         (["0,200,0"
           "0,0,200"
           "200,0,0"] a))
       ")"))

(defn render-neighborhood [renderer [_ path _ new-value] transmitter]
  (let [canvas (dom/by-id "neighborhood")
        ctx (.getContext canvas "2d")
        w (aget canvas "width")
        h (aget canvas "height")
        {:keys [width height neighborhood]} new-value]
    (.save ctx)
    (.scale ctx (/ w width) (/ h height))
    (doseq [x (range width)
            y (range height)
            :let [n (+ x (* y width))]]
      (aset ctx "fillStyle" (rgb-str (nth neighborhood n)))
      (.fillRect ctx x y 1 1))
    (.restore ctx)))

(def running? (atom nil))

(defn update-step-run-status [new-value]
  (let [op (if (empty? (:unhappy new-value))
             dom/add-class!
             dom/remove-class!)]
    (op (dom/by-id "step") "disabled")
    (op (dom/by-id "run") "disabled")))

(defn update-step-run-text []
  (dom/set-text! (dom/by-id "run") (if @running? "Stop" "Run")))

(defn trigger-step-if-running [input-queue]
  (when @running?
    (p/put-message input-queue {msg/topic :schelling-state msg/type :step})))

(defn schelling-state-changed [& args]
  (let [[_ [_ _ _ new-value] input-queue] args]
    (update-step-run-status new-value)
    (apply render-neighborhood args)
    (trigger-step-if-running input-queue)))

(defn running-changed [renderer [op path old-value new-value] input-queue]
  (reset! running? new-value)
  (update-step-run-text)
  (trigger-step-if-running input-queue))

;; The data structure below is used to map rendering data to functions
;; which handle rendering for that specific change. This function is
;; referenced in config/config.clj and must be a function in order to
;; be used from the tool's "render" view.

(defn render-config []
  [;; All :node-create deltas for the node at
   ;; :io.pedestal.app/view-example-transform will be rendered by the
   ;; `render-page` function. The node name
   ;; :io.pedestal.app/view-example-transform is a default name that is used
   ;; when we don't provide our own combines and emits. To name your
   ;; own nodes, create a custom combine or emit in the application's
   ;; behavior.
   ; [:node-create  [:io.pedestal.app/view-example-transform] render-page]
   [:node-create  [:view-schelling-state] render-page]
   ;; All :node-destroy deltas for this path will be handled by the
   ;; library function `d/default-exit`.
   [:node-destroy   [:io.pedestal.app/view-example-transform] d/default-exit]
   ;; All :value deltas for this path will be handled by the
   ;; function `render-message`.
   ; [:value [:io.pedestal.app/view-example-transform] render-message]
   [:value [:view-schelling-state] schelling-state-changed]
   [:value [:view-running?] running-changed]])

;; In render-config, paths can use wildcard keywords :* and :**. :*
;; means exactly one segment with any value. :** means 0 or more
;; elements.
