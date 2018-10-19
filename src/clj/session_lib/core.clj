(ns session-lib.core
  (:require [mongo-lib.core :as mon]
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
    (if-let [session-obj (mon/mongodb-find-one
                           cookie-name
                           {:uuid session-uuid})]
      (mon/mongodb-update-by-id
        cookie-name
        (:_id session-obj)
        {:created-at (java.util.Date.)})
      (let [session-obj {:uuid session-uuid
                         :user-agent user-agent
                         :user-id user-id
                         :username username
                         :created-at (java.util.Date.)}]
        (mon/mongodb-insert-one
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
                          :long-session)
                        -1)]
   (if-let [uuid (mon/mongodb-find-one
                   "session"
                   {:uuid session-uuid})]
     (if-let [preferences (mon/mongodb-find-one
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
     (if-let [uuid (mon/mongodb-find-one
                     "long-session"
                     {:uuid session-uuid})]
       (if-let [preferences (mon/mongodb-find-one
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
                            :long-session)
        user-agent (:user-agent request)
        [session-uuid
         cookie-name
         timeout-num] (if session-uuid
                        [session-uuid
                         "session"
                         session-timeout-num]
                        (when long-session-uuid
                          [long-session-uuid
                           "long-session"
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
      "long-session"
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
                                   :long-session)]
                   ["long-session"
                    uuid]))]
    (try
      (mon/mongodb-delete-by-filter
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
  (when (not (mon/mongodb-index-exists?
               "user"
               "username-unique-idx"))
    (mon/mongodb-create-index
      "user"
      {:username 1}
      "username-unique-idx"
      true))
  (when (not (mon/mongodb-index-exists?
               "user"
               "email-unique-idx"))
    (mon/mongodb-create-index
      "user"
      {:email 1}
      "email-unique-idx"
      true))
  (when (not (mon/mongodb-index-exists?
               "session"
               "short-session-idx"))
    (mon/mongodb-create-index
      "session"
      {:created-at 1}
      "short-session-idx"
      false
      session-timeout-num))
  (when (not (mon/mongodb-index-exists?
               "session"
               "session-uuid-unique-idx"))
    (mon/mongodb-create-index
      "session"
      {:uuid 1}
      "session-uuid-unique-idx"
      true))
  (when (not (mon/mongodb-index-exists?
               "long-session"
               "long-session-idx"))
    (mon/mongodb-create-index
      "long-session"
      {:created-at 1}
      "long-session-idx"
      false
      long-session-timeout-num))
  (when (not (mon/mongodb-index-exists?
               "long-session"
               "long-session-uuid-unique-idx"))
    (mon/mongodb-create-index
      "long-session"
      {:uuid 1}
      "long-session-uuid-unique-idx"
      true))
 )

(defn get-pass-for-email-username
  "Get password for supplied email"
  [email-username
   password]
  (if-let [user-username (mon/mongodb-find-one
                           "user"
                           {:username email-username})]
    (let [db-password (:password user-username)]
      (if (= db-password
             password)
        (if-let [preferences (mon/mongodb-find-one
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
    (if-let [user-email (mon/mongodb-find-one
                          "user"
                          {:email email-username})]
      (let [db-password (:password user-email)]
        (if (= db-password
               password)
          (if-let [preferences (mon/mongodb-find-one
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

