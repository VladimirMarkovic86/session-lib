(ns session-lib.core
  (:require [mongo-lib.core :as mon]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.response-header :as rsh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.status-code :as stc]
            [clojure.string :as cstring]
            [clojure.set :as cset]
            [common-middle.collection-names :refer [preferences-cname
                                                    session-cname
                                                    long-session-cname
                                                    session-visible-cname
                                                    long-session-visible-cname
                                                    user-cname
                                                    reset-password-cname]]))

(def session-timeout-num
     (* 30 60))

(def long-session-timeout-num
     (* 3 365 24 60 60))

(def reset-password-timeout-num
     (* 5 60))

(defn session-timeout
  "Return current date and time in particular format
   adding up seconds"
  [seconds]
  (when (and seconds
             (number?
               seconds))
    (let [simple-date-format (java.text.SimpleDateFormat.
                               "EE, dd MMM yyyy HH:mm:ss zzz")
          current-date-time (java.util.Date.)
          calendar (java.util.Calendar/getInstance)]
      (.setTime
        calendar
        current-date-time)
      (.add
        calendar
        java.util.Calendar/SECOND
        seconds)
      (.format
        simple-date-format
        (.getTime
          calendar))
     ))
 )

(defn session-cookie-string
  "Format session cookie with name, uuid, exparation datetime and max-age seconds"
  [cookie-name
   user-id
   username
   session-uuid
   timeout-in-seconds
   user-agent
   & [is-secure-on
      is-httponly-on
      is-persistent]]
  (when (and cookie-name
             (string?
               cookie-name)
             (not
               (cstring/blank?
                 cookie-name))
             session-uuid
             (string?
               session-uuid)
             (not
               (cstring/blank?
                 session-uuid))
             timeout-in-seconds
             (number?
               timeout-in-seconds))
    (try
      (when is-persistent
        (if-let [session-obj (mon/mongodb-find-one
                               cookie-name
                               {:uuid session-uuid})]
          (when (and (:_id session-obj)
                     (string?
                       (:_id session-obj))
                 )
            (mon/mongodb-update-by-id
              cookie-name
              (:_id session-obj)
              {:created-at (java.util.Date.)})
           )
          (when (and session-uuid
                     (string?
                       session-uuid)
                     user-agent
                     (string?
                       user-agent)
                     user-id
                     (string?
                       user-id)
                     username
                     (string?
                       username))
            (let [session-obj {:uuid session-uuid
                               :user-agent user-agent
                               :user-id user-id
                               :username username
                               :created-at (java.util.Date.)}]
              (mon/mongodb-insert-one
                cookie-name
                session-obj))
           ))
       )
      (str
        cookie-name
        "=" session-uuid "; "
        "Expires=" (session-timeout
                     timeout-in-seconds) "; "
        "Max-Age=" timeout-in-seconds "; "
        "Path=/; "
        ;"Domain=sample; "
        (when is-secure-on
          "Secure; ")
        (when is-httponly-on
          "HttpOnly; "))
      (catch Exception e
        (println e)
       ))
   ))

; Expires=Wed, 30 Aug 2019 00:00:00 GMT
; Max-age=5000
; Domain=localhost:1612
; Path=/
; Secure
; HttpOnly
; SameSite=Strict
; SameSite=Lax

(defn get-cookie
  "Read cookie from request
   cookies is string from cookie header from request
   cookie-name is cookie's name as keyword :cookie-name"
  [cookies
   cookie-name]
  (when-let [cookies cookies]
    (let [cookies (clojure.string/split
                    cookies
                    #"; ")
          cookies-map (atom {})]
      (doseq [cookie cookies]
        (let [[c-name
               c-value] (clojure.string/split
                          cookie #"=")]
          (swap!
            cookies-map
            assoc
            (keyword
              c-name)
            c-value))
       )
      (get
        @cookies-map
        cookie-name))
   ))

(defn get-accept-language
  "Read accept-language header for selected language"
  [request]
  (let [accept-language (:accept-language request)]
    (if accept-language
      (let [language-vector (cstring/split
                              accept-language
                              #",")
            first-choice-language (if (cstring/index-of
                                        (first
                                          language-vector)
                                        "sr")
                                    "sr"
                                    (when (cstring/index-of
                                            (first
                                              language-vector)
                                            "en")
                                      "en"))
            selected-language (if first-choice-language
                                first-choice-language
                                (let [language-set (into
                                                     #{}
                                                     language-vector)
                                      selected-language (cset/select
                                                          (fn [elem]
                                                            (cstring/index-of
                                                              elem
                                                              "sr"))
                                                            language-set)
                                      selected-language (if (empty?
                                                              selected-language)
                                                          "en"
                                                          (first
                                                            selected-language))]
                                  selected-language))]
        (if (cstring/index-of
              selected-language
              "sr")
          "serbian"
          "english"))
      "english"))
 )

(defn get-session-obj
  "Gets active session object"
  [request]
  (let [cookies (:cookie request)
        long-session-uuid (get-cookie
                            cookies
                            :long-session)
        long-session-obj (when long-session-uuid
                           (mon/mongodb-find-one
                             long-session-cname
                             {:uuid long-session-uuid}))
        session-uuid (get-cookie
                       cookies
                       :session)
        session-obj (when session-uuid
                      (mon/mongodb-find-one
                        session-cname
                        {:uuid session-uuid}))
        session-obj (or long-session-obj
                        session-obj
                        nil)]
    session-obj))

(defn get-preferences
  "Fetch preferences for logged in user"
  [request]
  (let [session-obj (get-session-obj
                      request)]
    (when session-obj
      (mon/mongodb-find-one
        preferences-cname
        {:user-id (:user-id session-obj)})
     ))
 )

(defn am-i-logged-in
 "Check if user is logged in"
 [request]
 (let [session-obj (get-session-obj
                     request)
       accepted-language (get-accept-language
                           request)
       accepted-language-name (cstring/capitalize
                                accepted-language)
       status-a (atom nil)
       body-a (atom nil)]
   (if session-obj
     (if-let [preferences (mon/mongodb-find-one
                            preferences-cname
                            {:user-id (:user-id session-obj)})]
       (do
         (reset!
           status-a
           (stc/ok))
         (reset!
           body-a
           {:status "It's ok"
            :username (:username session-obj)
            :language (:language preferences)
            :language-name (:language-name preferences)})
        )
       (do
        (reset!
           status-a
           (stc/ok))
         (reset!
           body-a
           {:status "It's ok"
            :username (:username session-obj)
            :language accepted-language
            :language-name accepted-language-name}))
      )
     (do
       (reset!
         status-a
         (stc/unauthorized))
       (reset!
         body-a
         "It's not ok"))
     )
    {:status @status-a
     :headers {(eh/content-type) (mt/text-clojurescript)}
     :body @body-a}))

(defn am-i-logged-in-fn
  "Shortcut function for result comparation of am-i-logged-in function
   if user is logged in"
  [request]
  (= (:status
       (am-i-logged-in
         request))
     (stc/ok))
 )

(defn refresh-session
  "Refreash existing session uuid for long or short session timeout"
  [{cookie :cookie
    user-agent :user-agent}]
  (when (and cookie
             (string?
               cookie)
             (not
               (cstring/blank?
                 cookie))
             user-agent
             (string?
               user-agent)
             (not
               (cstring/blank?
                 user-agent))
         )
    (let [long-session-uuid (get-cookie
                              cookie
                              :long-session)
          long-session-obj (when long-session-uuid
                             (mon/mongodb-find-one
                               long-session-cname
                               {:uuid long-session-uuid}))
          session-uuid (get-cookie
                         cookie
                         :session)
          session-obj (when session-uuid
                        (mon/mongodb-find-one
                          session-cname
                          {:uuid session-uuid}))
          result (atom nil)]
      (when long-session-obj
        (reset!
          result
          [(session-cookie-string
             long-session-cname
             nil
             nil
             long-session-uuid
             long-session-timeout-num
             user-agent
             true
             true
             true)
           (session-cookie-string
             (str
               long-session-cname
               "-visible")
             nil
             nil
             "exists"
             long-session-timeout-num
             nil)])
       )
      (when session-obj
        (reset!
          result
          [(session-cookie-string
             session-cname
             nil
             nil
             session-uuid
             session-timeout-num
             user-agent
             true
             true
             true)
           (session-cookie-string
             (str
               session-cname
               "-visible")
             nil
             nil
             "exists"
             session-timeout-num
             nil)])
       )
      @result))
 )

(defn session-cookie-string-fn
  "Format session cookie"
  [remember-me
   user
   uuid
   user-agent]
  (when (and user
             (map?
               user)
             (contains?
               user
               :_id)
             (contains?
               user
               :username)
             uuid
             (string?
               uuid)
             user-agent
             (string?
               user-agent))
    (if remember-me
      [(session-cookie-string
         long-session-cname
         (:_id user)
         (:username user)
         uuid
         long-session-timeout-num
         user-agent
         true
         true
         true)
       (session-cookie-string
         long-session-visible-cname
         nil
         nil
         "exists"
         long-session-timeout-num
         nil)]
      [(session-cookie-string
         session-cname
         (:_id user)
         (:username user)
         uuid
         session-timeout-num
         user-agent
         true
         true
         true)
       (session-cookie-string
         session-visible-cname
         nil
         nil
         "exists"
         session-timeout-num
         nil)])
   ))

(defn delete-session-record
  "Delete session record from database"
  [{cookies :cookie}]
  (if (and cookies
           (string?
             cookies)
           (not
             (cstring/blank?
               cookies))
       )
    (let [long-session-uuid (get-cookie
                              cookies
                              :long-session)
          long-session-obj (when long-session-uuid
                             (mon/mongodb-find-one
                               long-session-cname
                               {:uuid long-session-uuid}))
          session-uuid (get-cookie
                         cookies
                         :session)
          session-obj (when session-uuid
                        (mon/mongodb-find-one
                          session-cname
                          {:uuid session-uuid}))
          result (atom {:status (stc/internal-server-error)
                        :headers {(eh/content-type) (mt/text-plain)}
                        :body "Session cookies not present in request"})]
      (when long-session-obj
        (let [uuid (:uuid long-session-obj)
              destroy-cookie (session-cookie-string
                               long-session-cname
                               nil
                               nil
                               "destroyed"
                               0
                               nil
                               true
                               true)
              destroy-visible-cookie (session-cookie-string
                                       (str
                                         long-session-cname
                                         "-visible")
                                       nil
                                       nil
                                       "destroyed"
                                       0
                                       nil)]
          (try
            (mon/mongodb-delete-one
              long-session-cname
              {:uuid uuid})
            (reset!
              result
              {:status (stc/ok)
               :headers {(eh/content-type) (mt/text-plain)
                         (rsh/set-cookie) [destroy-cookie
                                           destroy-visible-cookie]
                         (rsh/set-visible-cookie) destroy-visible-cookie}
               :body "Bye bye"})
            (catch Exception e
              (println (.getMessage e))
              (reset!
                result
                {:status (stc/internal-server-error)
                 :headers {(eh/content-type) (mt/text-plain)}
                 :body "Stay for a little while"}))
           ))
       )
      (when session-obj
        (let [uuid (:uuid session-obj)
              destroy-cookie (session-cookie-string
                               session-cname
                               nil
                               nil
                               "destroyed"
                               0
                               nil
                               true
                               true)
              destroy-visible-cookie (session-cookie-string
                                       (str
                                         session-cname
                                         "-visible")
                                       nil
                                       nil
                                       "destroyed"
                                       0
                                       nil)]
          (try
            (mon/mongodb-delete-one
              session-cname
              {:uuid uuid})
            (reset!
              result
              {:status (stc/ok)
               :headers {(eh/content-type) (mt/text-plain)
                         (rsh/set-cookie) [destroy-cookie
                                           destroy-visible-cookie]
                         (rsh/set-visible-cookie) destroy-visible-cookie}
               :body "Bye bye"})
            (catch Exception e
              (println (.getMessage e))
              (reset!
                result
                {:status (stc/internal-server-error)
                 :headers {(eh/content-type) (mt/text-plain)}
                 :body "Stay for a little while"}))
           ))
       )
      @result)
    {:status (stc/internal-server-error)
     :headers {(eh/content-type) (mt/text-plain)}
     :body "Cookies not present in request"}))

(defn create-indexes
  "Create indexes for system to work"
  []
  (when-not (mon/mongodb-index-exists?
              user-cname
              "username-unique-idx")
    (mon/mongodb-create-index
      user-cname
      {:username 1}
      "username-unique-idx"
      true))
  (when-not (mon/mongodb-index-exists?
              user-cname
              "email-unique-idx")
    (mon/mongodb-create-index
      user-cname
      {:email 1}
      "email-unique-idx"
      true))
  (when-not (mon/mongodb-index-exists?
              session-cname
              "short-session-idx")
    (mon/mongodb-create-index
      session-cname
      {:created-at 1}
      "short-session-idx"
      false
      session-timeout-num))
  (when-not (mon/mongodb-index-exists?
              session-cname
              "session-uuid-unique-idx")
    (mon/mongodb-create-index
      session-cname
      {:uuid 1}
      "session-uuid-unique-idx"
      true))
  (when-not (mon/mongodb-index-exists?
              long-session-cname
              "long-session-idx")
    (mon/mongodb-create-index
      long-session-cname
      {:created-at 1}
      "long-session-idx"
      false
      long-session-timeout-num))
  (when-not (mon/mongodb-index-exists?
              long-session-cname
              "long-session-uuid-unique-idx")
    (mon/mongodb-create-index
      long-session-cname
      {:uuid 1}
      "long-session-uuid-unique-idx"
      true))
  (when-not (mon/mongodb-index-exists?
              reset-password-cname
              "reset-password-idx")
    (mon/mongodb-create-index
      reset-password-cname
      {:created-at 1}
      "reset-password-idx"
      false
      reset-password-timeout-num))
  (when-not (mon/mongodb-index-exists?
              reset-password-cname
              "reset-password-uuid-unique-idx")
    (mon/mongodb-create-index
      reset-password-cname
      {:uuid 1}
      "reset-password-uuid-unique-idx"
      true))
  (when-not (mon/mongodb-index-exists?
              reset-password-cname
              "reset-password-email-unique-idx")
    (mon/mongodb-create-index
      reset-password-cname
      {:email 1}
      "reset-password-email-unique-idx"
      true))
 )

(defn get-pass-for-email-username
  "Get password for supplied email"
  [email-username
   password
   & [accept-language]]
  (if (and email-username
           (string?
             email-username)
           (not
             (cstring/blank?
               email-username))
           password
           (string?
             password)
           (not
             (cstring/blank?
               password))
       )
    (let [accepted-language (get-accept-language
                              {:accept-language accept-language})
          accepted-language-name (cstring/capitalize
                                   accepted-language)]
      (if-let [user-username (mon/mongodb-find-one
                               user-cname
                               {:username email-username})]
        (let [db-password (:password user-username)]
          (if (= db-password
                 password)
            (if-let [preferences (mon/mongodb-find-one
                                   preferences-cname
                                   {:user-id (:_id user-username)})]
              [{:status "success"
                :email "success"
                :password "success"
                :username (:username user-username)
                :language (:language preferences)
                :language-name (:language-name preferences)}
               user-username]
              [{:status "success"
                :email "success"
                :password "success"
                :username (:username user-username)
                :language accepted-language
                :language-name accepted-language-name}
               user-username])
            [{:status "error"
              :email "success"
              :password "error"}]))
        (if-let [user-email (mon/mongodb-find-one
                              user-cname
                              {:email email-username})]
          (let [db-password (:password user-email)]
            (if (= db-password
                   password)
              (let [preferences (mon/mongodb-find-one
                                  preferences-cname
                                  {:user-id (:_id user-email)})]
                (if (and preferences
                         (map?
                           preferences)
                         (not
                           (empty?
                             preferences))
                     )
                  [{:status "success"
                    :email "success"
                    :password "success"
                    :username (:username user-email)
                    :language (:language preferences)
                    :language-name (:language-name preferences)}
                   user-email]
                  [{:status "success"
                    :email "success"
                    :password "success"
                    :username (:username user-email)
                    :language accepted-language
                    :language-name accepted-language-name}
                   user-email]))
              [{:status "error"
                :email "success"
                :password "error"}]))
          [{:status "error"
            :email "error"
            :password "error"}]))
     )
    [{:status "error"
      :email "error"
      :password "error"}]))

(defn login-authentication
  "Login authentication"
  [{user-agent :user-agent
    accept-language :accept-language
    {email-username :email
     password :password
     remember-me :remember-me} :body}]
  (if (and email-username
           (string?
             email-username)
           (not
             (cstring/blank?
               email-username))
           password
           (string?
             password)
           (not
             (cstring/blank?
               password))
           user-agent
           (string?
             user-agent)
           (not
             (cstring/blank?
               user-agent))
       )
    (try
      (let [[result
             user] (get-pass-for-email-username
                     email-username
                     password
                     accept-language)
            headers (atom
                      {(eh/content-type) (mt/text-clojurescript)})
            status-code (atom (stc/ok))]
        (if (= (:status result)
               "success")
          (let [uuid (.toString
                       (java.util.UUID/randomUUID))
                [session-cookie
                 session-visible-cookie] (session-cookie-string-fn
                                           remember-me
                                           user
                                           uuid
                                           user-agent)]
            (if session-cookie
              (swap!
                headers
                assoc
                (rsh/set-cookie)
                [session-cookie
                 session-visible-cookie]
                (rsh/set-visible-cookie)
                session-visible-cookie)
              (reset!
                status-code
                (stc/internal-server-error))
             ))
          (reset!
            status-code
            (stc/unauthorized))
         )
        {:status @status-code
         :headers @headers
         :body result})
      (catch Exception e
        (println
          (.getMessage
            e))
        {:status (stc/internal-server-error)
         :headers {(eh/content-type) (mt/text-clojurescript)}
         :body {:status "error"}}))
    {:status (stc/internal-server-error)
     :headers {(eh/content-type) (mt/text-clojurescript)}
     :body {:status "error"}}))

(defn logout
  "Logout user from system"
  [request]
  (delete-session-record
    request))

(defn set-session-cookies
  "Set session cookies"
  [request
   response]
  (when (and request
             (map?
               request)
             response
             (map?
               response))
    (let [response (atom response)
          [cookie-value
           visible-cookie-value] (refresh-session
                                   request)]
      (if (and cookie-value
               (string?
                 cookie-value)
               (not
                 (cstring/blank?
                   cookie-value))
               visible-cookie-value
               (string?
                 visible-cookie-value)
               (not
                 (cstring/blank?
                   visible-cookie-value))
           )
        (let [set-cookies-header (get-in
                                   @response
                                   [:headers
                                    (rsh/set-cookie)])]
          (when (vector?
                  set-cookies-header)
            (reset!
              response
              (update-in
                @response
                [:headers
                 (rsh/set-cookie)]
                conj
                cookie-value
                visible-cookie-value))
            (reset!
              response
              (update-in
                @response
                [:headers]
                assoc
                (rsh/set-visible-cookie)
                visible-cookie-value))
           )
          (when (and (string?
                       set-cookies-header)
                     (not
                       (cstring/blank?
                         set-cookies-header))
                 )
            (reset!
              response
              (update-in
                @response
                [:headers]
                assoc
                (rsh/set-cookie)
                [set-cookies-header
                 cookie-value
                 visible-cookie-value]
                (rsh/set-visible-cookie)
                visible-cookie-value))
           )
          (when (nil?
                  set-cookies-header)
            (reset!
              response
              (update-in
                @response
                [:headers]
                assoc
                (rsh/set-cookie)
                [cookie-value
                 visible-cookie-value]
                (rsh/set-visible-cookie)
                visible-cookie-value))
           )
          (when (not
                  (or (and (string?
                             set-cookies-header)
                           (not
                             (cstring/blank?
                               set-cookies-header))
                       )
                      (vector?
                        set-cookies-header)
                      (nil?
                        set-cookies-header))
                 )
            (reset!
              response
              (update-in
                @response
                [:headers]
                assoc
                (rsh/set-cookie)
                [cookie-value
                 visible-cookie-value]
                (rsh/set-visible-cookie)
                visible-cookie-value))
           )
          @response)
        @response))
   ))

