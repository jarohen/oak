(ns oak.http
  (:require [oak.async :as a]
            [oak.core :as o]
            [cljs-http.client :as http]))

(defn request-cmd [{:keys [method url ev] :as opts}]
  (->> (a/async-cmd (http/request (dissoc opts :ev)))
       (o/fmap-cmd (fn [{:keys [success] :as resp}]
                     (merge ev
                            {:resp (dissoc resp :success)
                             :success? success})))))
