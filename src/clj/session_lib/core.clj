(ns session-lib.core
  (:require [db-lib.core :as db]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.response-header :as rsh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.status-code :as stc]))

(def session-timeout-num
     (* 30 60))

(def long-session-timeout-num
     (* 3 365 24 60 60))

(defn session-timeout
  "Return current date and time in particular format
   adding up seconds"
  [seconds]
  (let [simple-date-format (java.text.SimpleDateFormat. "EE, dd MMM yyyy HH:mm:ss zzz")
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

(defn session-cookie-string
  "Format session cookie with name, uuid, exparation datetime and max-age seconds"
  [cookie-name
   user-id
   username
   session-uuid
   timeout-in-seconds
   user-agent]
  (try
    (if-let [session-obj (db/find-one-by-filter
                           cookie-name
                           {:uuid session-uuid})]
      (db/update-by-id
        cookie-name
        (:_id session-obj)
        {:created-at (java.util.Date.)})
      (let [session-obj {:uuid session-uuid
                         :user-agent user-agent
                         :user-id user-id
                         :username username
                         :created-at (java.util.Date.)}]
        (db/insert-one
          cookie-name
          session-obj))
     )
    (str
      cookie-name
      "=" session-uuid "; "
      "Expires=" (session-timeout
                   timeout-in-seconds) "; "
      "Max-Age=" timeout-in-seconds "; "
      "Path=/; "
      ;"Domain=sample; "
      ;"Secure; "
      ;"HttpOnly"
     )
    (catch Exception e
      (println (.getMessage e))
     ))
 )

; Expires=Wed, 30 Aug 2019 00:00:00 GMT
; Max-age=5000
; Domain=localhost:1612
; Path=/
; Secure
; HttpOnly
; SameSite=Strict
; SameSite=Lax

(defn get-cookie
  "Read cookie from request"
  [cookies
   cookie-name]
  (when-let [cookies cookies]
    (let [cookies (clojure.string/split cookies #"; ")
          cookies-map (atom {})]
      (doseq [cookie cookies]
        (let [[c-name
               c-value] (clojure.string/split cookie #"=")]
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

(defn am-i-logged-in
 "Check if user is logged in"
 [request]
 (let [cookies (:cookie request)
       session-uuid (or (get-cookie
                          cookies
                          :session)
                        (get-cookie
                          cookies
                          :long_session)
                        -1)]
   (if-let [uuid (db/find-one-by-filter
                   "session"
                   {:uuid session-uuid})]
     (if-let [preferences (db/find-one-by-filter
                            "preferences"
                            {:user-id (:user-id uuid)})]
       {:status (stc/ok)
        :headers {(eh/content-type) (mt/text-plain)}
        :body (str
                {:status "It's ok"
                 :username (:username uuid)
                 :language-name (:language-name preferences)})}
       {:status (stc/ok)
        :headers {(eh/content-type) (mt/text-plain)}
        :body (str
                {:status "It's ok"
                 :username (:username uuid)
                 :language-name "English"})})
     (if-let [uuid (db/find-one-by-filter
                     "long_session"
                     {:uuid session-uuid})]
       (if-let [preferences (db/find-one-by-filter
                              "preferences"
                              {:user-id (:user-id uuid)})]
         {:status (stc/ok)
          :headers {(eh/content-type) (mt/text-plain)}
          :body (str
                  {:status "It's ok"
                   :username (:username uuid)
                   :language-name (:language-name preferences)})}
         {:status (stc/ok)
          :headers {(eh/content-type) (mt/text-plain)}
          :body (str
                  {:status "It's ok"
                   :username (:username uuid)
                   :language-name "English"})}
        )
       {:status (stc/unauthorized)
        :headers {(eh/content-type) (mt/text-plain)}
        :body "It's not ok"}))
  ))

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
  [request]
  (let [session-uuid (get-cookie
                       (:cookie request)
                       :session)
        long-session-uuid (get-cookie
                            (:cookie request)
                            :long_session)
        user-agent (:user-agent request)
        [session-uuid
         cookie-name
         timeout-num] (if session-uuid
                        [session-uuid
                         "session"
                         session-timeout-num]
                        (when long-session-uuid
                          [long-session-uuid
                           "long_session"
                           long-session-timeout-num]))]
    [(rsh/set-cookie)
     (session-cookie-string
       cookie-name
       nil
       nil
       session-uuid
       timeout-num
       user-agent)])
 )

(defn session-cookie-string-fn
  "Format session cookie"
  [remember-me
   user
   uuid
   user-agent]
  (if remember-me
    (session-cookie-string
      "long_session"
      (:_id user)
      (:username user)
      uuid
      long-session-timeout-num
      user-agent)
    (session-cookie-string
      "session"
      (:_id user)
      (:username user)
      uuid
      session-timeout-num
      user-agent))
 )

(defn delete-session-record
  "Delete session record from database"
  [request]
  (let [cookies (:cookie request)
        [cookie-name
         uuid] (if-let [uuid (get-cookie
                               cookies
                               :session)]
                 ["session"
                  uuid]
                 (when-let [uuid (get-cookie
                                   cookies
                                   :long_session)]
                   ["long_session"
                    uuid]))]
    (try
      (db/delete-by-filter
        cookie-name
        {:uuid uuid})
      {:status (stc/ok)
       :headers {(eh/content-type) (mt/text-plain)}
       :body "Bye bye"}
      (catch Exception e
        (println (.getMessage e))
        {:status (stc/internal-server-error)
         :headers {(eh/content-type) (mt/text-plain)}
         :body "Stay for a little while"}))
   ))

(defn create-indexes
  "Create indexes for system to work"
  []
  (when (not (db/index-exists?
               "user"
               "username-unique-idx"))
    (db/create-unique-index
      "user"
      [:username]
      "username-unique-idx"))
  (when (not (db/index-exists?
               "user"
               "email-unique-idx"))
    (db/create-unique-index
      "user"
      [:email]
      "email-unique-idx"))
  (when (not (db/index-exists?
               "session"
               "short-session-idx"))
    (db/create-ttl-index
      "session"
      :created-at
      "short-session-idx"
      session-timeout-num))
  (when (not (db/index-exists?
               "session"
               "session-uuid-unique-idx"))
    (db/create-unique-index
      "session"
      [:uuid]
      "session-uuid-unique-idx"))
  (when (not (db/index-exists?
               "long_session"
               "long-session-idx"))
    (db/create-ttl-index
      "long_session"
      :created-at
      "long-session-idx"
      long-session-timeout-num))
  (when (not (db/index-exists?
               "long_session"
               "long-session-uuid-unique-idx"))
    (db/create-unique-index
      "long_session"
      [:uuid]
      "long-session-uuid-unique-idx"))
 )

(defn get-pass-for-email-username
  "Get password for supplied email"
  [email-username
   password]
  (if-let [user-username (db/find-one-by-filter
                           "user"
                           {:username email-username})]
    (let [db-password (:password user-username)]
      (if (= db-password
             password)
        (if-let [preferences (db/find-one-by-filter
                               "preferences"
                               {:user-id (:_id user-username)})]
          [{:status "success"
            :email "success"
            :password "success"
            :username (:username user-username)
            :language-name (:language-name preferences)}
           user-username]
          [{:status "success"
            :email "success"
            :password "success"
            :username (:username user-username)
            :language-name "English"}
           user-username])
        [{:status "error"
          :email "success"
          :password "error"}]))
    (if-let [user-email (db/find-one-by-filter
                          "user"
                          {:email email-username})]
      (let [db-password (:password user-email)]
        (if (= db-password
               password)
          (if-let [preferences (db/find-one-by-filter
                                 "preferences"
                                 {:user-id (:_id email-username)})]
            [{:status "success"
              :email "success"
              :password "success"
              :username (:username user-email)
              :language-name (:language-name preferences)}
             user-email]
            [{:status "success"
              :email "success"
              :password "success"
              :username (:username user-email)
              :language-name "English"}
             user-email])
          [{:status "error"
            :email "success"
            :password "error"}]))
      [{:status "error"
        :email "error"
        :password "error"}]))
 )

(defn login-authentication
  "Login authentication"
  [request-body
   user-agent]
  (let [email-username (:email request-body)
        password (:password request-body)
        remember-me (:remember-me request-body)
        [result
         user] (get-pass-for-email-username
                 email-username
                 password)]
    (if (= (:status result)
           "success")
      (let [uuid (.toString (java.util.UUID/randomUUID))
            session-cookie (session-cookie-string-fn
                             remember-me
                             user
                             uuid
                             user-agent)]
        (if session-cookie
          {:status (stc/ok)
           :headers {(eh/content-type) (mt/text-plain)
                     (rsh/set-cookie) session-cookie}
           :body (str result)}
          {:status (stc/internal-server-error)
           :headers {(eh/content-type) (mt/text-plain)}
           :body (str result)})
       )
      {:status (stc/unauthorized)
       :headers {(eh/content-type) (mt/text-plain)}
       :body (str result)})
   ))

(defn logout
  "Logout user from system"
  [request]
  (delete-session-record
    request))

