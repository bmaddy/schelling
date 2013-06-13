(ns ^:shared schelling-app.behavior
    (:require [clojure.string :as string]
              [io.pedestal.app :as pedestal]
              [io.pedestal.app.messages :as msg]
              [schelling-app.model :as s]))

;; While creating new behavior, write tests to confirm that it is
;; correct. For examples of various kinds of tests, see
;; test/schelling_app/test/behavior.clj.

;; You'll always receive a message with the type msg/init when your
;; app starts up. This message will include a :value key with the
;; value of the :init key from your dataflow.

(defn example-transform [transform-state message]
  (condp = (msg/type message)
    msg/init (:value message)
    :change (:value message)
    transform-state))

(defn message-logger [transform-fn]
  (fn [transform-state message]
    (.log js/console (str "message: " transform-state ": " (pr-str message)))
    (transform-fn transform-state message)))

(defn schelling-transform [old-model {value :value :as message}]
  (condp = (msg/type message)
    :setup (try (s/setup value))
             ; (catch js/Object e
             ;   (.dir js/console e)
             ;   (js/alert (str "Setup Error: " (aget e "message")))))
    :step (s/step old-model)
    old-model))

(defn running-transform [old-model message]
  (condp = (msg/type message)
    :toggle (not old-model)
    :stop false
    old-model))

(defn stop-running-when-finished [input-name old-value new-value]
  (when (empty? (:unhappy new-value))
    [{msg/topic :running? msg/type :stop}]))

(defn pass-through-combine [state input-name old-model new-model]
  (.log js/console "hit combine")
  new-model)

(defn emit-fn [inputs changed-inputs]
  (.log js/console (pr-str inputs))
  (.log js/console (pr-str inputs)))

(def example-app
  {:transform {:schelling-state {:init nil :fn schelling-transform}
               :running? {:init false :fn (message-logger running-transform)}}
   :combine {:continue-running? {:fn pass-through-combine :input #{:schelling-state}}
             :view-running? {:fn pass-through-combine :input #{:running?}}
             :view-schelling-state {:fn pass-through-combine :input #{:schelling-state}}}
   :continue {:continue-running? stop-running-when-finished}
   })


;; Once this behavior works, run the Data UI and record
;; rendering data which can be used while working on a custom
;; renderer. Rendering involves making a template:
;;
;; app/templates/schelling-app.html
;;
;; slicing the template into pieces you can use:
;;
;; app/src/schelling_app/html_templates.cljs
;;
;; and then writing the rendering code:
;;
;; app/src/schelling_app/rendering.cljs


(comment

  ;; The examples below show the signature of each type of function
  ;; that is used to build a behavior dataflow.

  ;; transform

  (defn example-transform [transform-state message]
    ;; returns new state
    )

  ;; effect

  (defn example-effect [message old-transform-state new-transform-state]
    ;; returns vector of messages to be added to input queue for future processing
    )

  ;; combine

  (defn example-combine-1 [combine-state input-name old-transform-state new-transform-state]
    ;; returns new combine state
    )

  (defn example-combine-2 [combine-state inputs]
    ;; inputs are a map of input names to their old and new state
    ;; returns new combine state
    )

  ;; continue

  (defn example-continue [combine-name old-combine-state new-combine-state]
    ;; returns vector of messages to be processed as part of current data flow execution
    )

  ;; emit

  (defn example-emit
    ([input]
       ;; input is a map of input names to their old and new state
       ;; called when emit is first displayed - returns rendering data
       )
    ([input changed-input]
       ;; input is a map of input names to their old and new state
       ;; changed-input is a set of the input names which have changed
       ;; called when inputs are updated - returns rendering data
       ))

  ;; example dataflow map

  {:transform {:example-transform {:init "" :fn example-transform}}
   :effect {:example-transform example-effect}
   :combine {:example-combine {:fn example-combine-1 :input #{:example-transform}}}
   :continue {:example-combine example-continue}
   :emit {:example-emit {:fn example-emit :input #{:example-combine}}}
   :focus {:home [[:a-path]]
                :default :home}}

  )
