(ns beetle.middleware.rate-limit
  "A clj-http middleware for rate limiting requests to the same domain(s)."
  {:authors ["Reid \"arrdem\" McKenzie <me@arrdem.com>"],
   :license "https://www.eclipse.org/legal/epl-v10.html"}
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as str]
            [rate-gate.core :refer [rate-gate tarry]]))

;; Rate limit middleware
;;--------------------------------------------------------------------------------------------------
(def ^{:dynamic true
       :doc "A DNS domain structure trie of rate gates."}
  *rate-limits*
  (atom {}))

(defn server-name->rate-limit [limits name]
  (let [paths (take-while not-empty (iterate butlast (reverse (str/split name #"\."))))]
    (first (keep #(get-in limits %) paths))))

(def ^{:dynamic true
       :doc "A 0-arity factory function for rate gates."}
  *default-rate-limit-fn*
  (fn []
    (rate-gate 1 1000)))

(defn get-or-create-rate-limit [limits server]
  (if (get-in limits server)
    limits
    (if *default-rate-limit-fn*
      (assoc-in limits server (*default-rate-limit-fn*))
      limits)))

(defn wrap-rate-limiting-middleware
  "Function of a client which checks the server a request targets and
  rate gates requests on a per-service basis.

  Rate limits are maintained as shared state in `*rate-limits*`, and
  `*default-rate-limit-fn*` is used to hold a factory for new default
  rate limits.

  If `*rate-limits*` is `nil`, no limits are applied.

  If `*default-rate-limit-fn*` is `nil`, no rate limit will be applied
  to domains which do not already have a listed rate limit."
  [client]
  (fn [{:keys [server-name] :as req}]
    (when-let [gates (when *rate-limits*
                  (swap! *rate-limits* get-or-create-rate-limit
                         (reverse (str/split server-name #"\."))))]
      (when-let [gate (server-name->rate-limit gates server-name)]
        (tarry gate)))

    ;; propagate the request
    (client req)))
