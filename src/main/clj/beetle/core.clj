(ns beetle.core
  "Tools for crawling & HTTP."
  {:authors ["Reid \"arrdem\" McKenzie <me@arrdem.com>"]
   :license "https://www.eclipse.org/legal/epl-v10.html"}
  (:refer-clojure :exclude [get])
  (:require [clj-http.util :as http-util]
            [clj-http.client :as http]
            [clojure.string :as str])
  (:import java.net.URL))

(defmacro when-pos [x]
  `(let [x# ~x]
     (if (pos? x#) x# nil)))

(defn parse-query [^String query-str]
  (when query-str
    (->> (str/split query-str #"&")
         (map (comp (fn [[k v]]
                      [k (http-util/url-decode v)])
                    #(str/split % #"=")))
         (into {:type ::query}))))

(defn query? [o]
  (and (map? o)
       (= (:type o) ::query)))

(defn parse-url
  "Parse a URL string into a map of interesting parts.

  Altered from `#'clj-http.client/parse-url`"
  [url]
  (if-let [url-parsed (URL. url)]
    {:type         ::url
     :scheme       (keyword (.getProtocol url-parsed))
     :server-name  (.getHost url-parsed)
     :server-port  (when-pos (.getPort url-parsed))
     :uri          (.getPath url-parsed)
     :user-info    (if-let [user-info (.getUserInfo url-parsed)]
                     (http-util/url-decode user-info))
     :query-params (parse-query (.getQuery url-parsed))}))

(defn url? [o]
  (and (map? o)
       (= (:type o) ::url)))

(defn unparse-query [query-map]
  (->> (dissoc query-map :type)
       (keep (fn [[k v]]
               (when v
                 (format "%s=%s" k (http-util/url-encode v)))))
       (str/join "&")
       (str "?")))

(defn unparse-url
  "Takes a map of url-parts and generates a string representation.

  Altered from `#'clj-http.client/unparse-url`"
  [{:keys [scheme server-name server-port uri user-info query-params]}]
  (str (name scheme) "://"
       (if (seq user-info)
         (str user-info "@" server-name)
         server-name)
       (when server-port
         (str ":" server-port))
       uri
       (if (query? query-params)
         (unparse-query query-params)
         (if (string? query-params)
           (str "?" query-params)))))

(defn ->req [{:keys [url query-params] :as req}]
  (let [url (if (string? url)
              (parse-url url)
              (if (url? url)
                url
                (throw (ex-info "Accepts only a string or a beetle URL for the URL part"
                                {:url url
                                 :type (type url)}))))
        query-params (when query-params
                       (if (string? query-params)
                         (parse-query query-params)
                         (if (map? query-params)
                           query-params
                           (throw (ex-info "Accepts only a string or a beetle params for the query-params part"
                                           {:params query-params
                                            :type (type query-params)})))))]
    (merge req
           {:type ::request
            :url (dissoc url :query-params)
            :query-params (->> (merge (:query-params url) query-params)
                               (keep (fn [[k v :as e]]
                                       (when-not (nil? v) e)))
                               (into {}))})))

(defn request? [o]
  (and (map? o)
       (= (:type o) ::request)))

(defmacro defwrapper [method]
  `(defn ~method [{:keys [~'url] :as ~'request}]
     {:pre [(request? ~'request)]}
     (~(symbol "http" (name method)) (unparse-url ~'url) ~'request)))

(defwrapper put)
(defwrapper post)
(defwrapper delete)
(defwrapper options)
(defwrapper copy)
(defwrapper move)
(defwrapper patch)
(defwrapper get)
