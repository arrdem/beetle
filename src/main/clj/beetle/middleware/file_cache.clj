(ns beetle.middleware.file-cache
  "A clj-http middleware for maintaining a somewhat RFC compliant response cache."
  {:authors ["Reid \"arrdem\" McKenzie <me@arrdem.com>"],
   :license "https://www.eclipse.org/legal/epl-v10.html"}
  (:require [beetle.middleware.rate-limit :refer [*rate-limits*]]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [pandect.algo.sha256 :as sha]
            [taoensso.nippy :as nippy])
  (:import java.io.File
           java.nio.charset.StandardCharsets
           [java.text DateFormat SimpleDateFormat]
           java.util.Date))

;; File cache middleware
;;
;; Built off of the example cache middleware
;;--------------------------------------------------------------------------------------------------
(defn req->f [cache-root {:keys [server-name uri query-string] :as req}]
  (let [[_ ext] (when uri (re-find #".*?\.(.*)\Z" uri))
        ext     (or ext "html")]
    (.mkdirs cache-root)
    (io/file cache-root (format "%s.%s" (sha/sha256 (str server-name uri query-string)) ext))))

(defn slurp-bytes
  "Read all bytes from the stream.
  Use for example when the bytes will be in demand after stream has been closed."
  [stream]
  (.getBytes (slurp stream) StandardCharsets/UTF_8))

(def if-modified-since-format
  (SimpleDateFormat. "E, d MMM y HH:mm:ss 'GMT'"))

(defn utc-normalize ^long [^long ms]
  (let [tz (java.util.TimeZone/getDefault)
        cal (java.util.GregorianCalendar/getInstance tz)]
    (- ms (. tz (getOffset (. cal (getTimeInMillis)))))))

(defn if-modified-since-date [^File f]
  (.format ^DateFormat if-modified-since-format
           (Date. (utc-normalize (.lastModified f)))))

(defn- cached-response
  "Look up the response in the cache using URL as the cache key.
  If the cache has the response, return the cached value.
  If the cache does not have the response, invoke the remaining middleware functions
  to perform the request and receive the response.
  If the response is successful (2xx) and is a GET, store the response in the cache.
  Return the response."
  [cache-rooot client req]
  (let [method  (or (:method req) (:request-method req))
        cache-f (req->f cache-rooot req)]
    (if (and (= :get method)
             (:cache req true)
             (.exists cache-f))
      (do (log/infof "Cache hit on %s%s?%s" (:server-name req) (:uri req) (:query-string req))
          ;; The cache ignores URL parameters and some other stuff.  The correct "real browser"
          ;; behavior is to issue a request to the server, and only hit in the cache if the server
          ;; says to by returning a 304 response.
          (let [date (if-modified-since-date cache-f)
                _    (log/infof "Checking to see if resource has changed since '%s'" date)
                req* (update req :headers merge
                                     {"if-modified-since" date
                                      "cache-control"     "max-age=120"})
                resp (binding [*rate-limits* nil]
                       (client req*))]
            (if (= 304 (:status resp))
              (do (log/info "Server responded 304 unchanged")
                  (nippy/thaw-from-file cache-f))

              (let [resp (update resp :body slurp-bytes)]
                (if (http/success? resp)
                  (nippy/freeze-to-file cache-f resp))

                (log/infof "Server returned %s changed for %s%s" (:status resp)
                           (:server-name req) (:uri req))
                resp))))
      (do (log/infof "Cache miss on %s%s?%s" (:server-name req) (:uri req) (:query-string req))
          (let [resp (update (client req) :body slurp-bytes)]
            (when (and (http/success? resp) (= :get method))
              (nippy/freeze-to-file cache-f resp))
            resp)))))

(defn ->caching-middleware
  "Middleware construtor.

  Takes a path as a string or a `File` object, returning a clj-http
  middleware which will record all `GET` requests to the file cache
  and send `if-modified-since` requests when a cache entry is
  available for the given request.

  Clients may opt out of caching behavior by setting `:cache false` in
  their requests.

  Unless caching is forced off, re-hits within 60s ALWAYS hit in the
  cache.

  The cache is maintained as a bunch of nippy which is never expired."
  [cache-root]
  (fn [client]
    (fn [req]
      (cached-response (io/file cache-root) client req))))
