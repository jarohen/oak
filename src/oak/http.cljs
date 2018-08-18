(ns oak.http
  (:require [oak.core :as oak]
            [cljs-http.client :as http]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defmethod oak/cmd! ::request! [{:keys [method url ev] :as opts} cb]
  (go
    (let [{:keys [success] :as resp} (a/<! (http/request (dissoc opts :ev)))]
      (when-let [[ev-type ev-args] ev]
        (cb [ev-type (merge ev-args
                            {::resp (dissoc resp :success)
                             ::success? success})])))))
