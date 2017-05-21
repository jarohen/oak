(ns oak.async
  (:require [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defn async-cmd [<ch]
  (fn [cb]
    (go-loop []
      (when-let [msg (a/<! <ch)]
        (cb msg)
        (recur)))))
