(ns oak.nav.bidi
  (:require [oak.nav :as nav]
            [bidi.bidi :as bidi]))

(defrecord Router [routes]
  nav/Router
  (parse [_ url-path]
    (bidi/match-route routes url-path))

  (unparse [_ handler route-params]
    (or (bidi/unmatch-pair routes
                           {:handler handler
                            :params route-params})
        (throw (ex-info "Can't unparse location:" {:handler handler
                                                   :route-params route-params})))))
