(ns oak.reagent
  (:require [oak.core :as o]
            [reagent.core :as r]))

(defn render! [{:keys [$el initial-state component handle-ev]}]
  (let [!ctx (r/atom nil)]
    (reset! !ctx (o/->ctx initial-state
                          {:swap-ctx! #(apply swap! !ctx %&)
                           :handle-ev handle-ev}))

    (o/handle-cmds! @!ctx)

    (r/render-component [(fn []
                           [component @!ctx])]
                        $el)))
