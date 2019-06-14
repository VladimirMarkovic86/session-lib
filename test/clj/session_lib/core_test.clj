(ns session-lib.core-test
  (:require [clojure.test :refer :all]
            [session-lib.core :refer :all]
            [mongo-lib.core :refer :all]
            [clojure.string :as cstring]
            [ajax-lib.http.status-code :as stc]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.response-header :as rsh]))

(def db-uri
     (or (System/getenv "MONGODB_URI")
         (System/getenv "PROD_MONGODB")
         "mongodb://admin:passw0rd@127.0.0.1:27017/admin"))

(def db-name
     "test-db")

(defn create-db
  "Create database for testing"
  []
  (mongodb-connect
    db-uri
    db-name)
  (mongodb-insert-many
    "user"
    [{ :username "test-user"
	      :email "234@234" }])
	 (let [user-db-obj (mongodb-find-one
	                     "user"
	                     {:username "test-user"})
	       _id (:_id user-db-obj)]
	   (mongodb-insert-many
      "session"
      [{:uuid "test-uuid"
        :user-agent "Test browser"
        :user-id _id
        :username "admin"
        :created-at (java.util.Date.)}
       {:uuid "4321-uuid"
        :user-agent "Test browser"
        :user-id _id
        :username "admin"
        :created-at (java.util.Date.)}
       {:uuid "1234-uuid"
        :user-agent "Test browser"
        :user-id _id
        :username "admin"
        :created-at (java.util.Date.)}
       ])
	   (mongodb-insert-many
      "long-session"
      [{:uuid "4321-uuid"
        :user-agent "Test browser"
        :user-id _id
        :username "admin"
        :created-at (java.util.Date.)}
       {:uuid "1234-uuid"
        :user-agent "Test browser"
        :user-id _id
        :username "admin"
        :created-at (java.util.Date.)}
       ])
    (mongodb-insert-many
      "preferences"
      [{:user-id _id
        :language "serbian"
        :language-name "Serbian"}
       ])
   ))

(defn destroy-db
  "Destroy testing database"
  []
  (mongodb-drop-database
    "test-db")
  (mongodb-disconnect))

(defn before-and-after-tests
  "Before and after tests"
  [f]
  (create-db)
  (f)
  (destroy-db))

(use-fixtures :each before-and-after-tests)

(deftest test-session-timeout
  (testing "Test session timeout"
    
    (let [result (session-timeout
                   nil)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [simple-date-format (java.text.SimpleDateFormat.
                               "EE, dd MMM yyyy HH:mm:ss zzz")
          result (session-timeout
                   10)]
      
      (is
        (< (- (.getTime
                (.parse
                  simple-date-format
                  result))
              (.getTime
                (java.util.Date.))
            )
           10000)
       )
      
     )
    
   ))

(deftest test-session-cookie-string
  (testing "Test session cookie string"
    
    (let [cookie-name nil
          user-id nil
          username nil
          session-uuid nil
          timeout-in-seconds nil
          user-agent nil
          is-secure-on nil
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid nil
          timeout-in-seconds nil
          user-agent nil
          is-secure-on nil
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "1234-uuid"
          timeout-in-seconds nil
          user-agent nil
          is-secure-on nil
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "1234-uuid"
          timeout-in-seconds ""
          user-agent nil
          is-secure-on nil
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on nil
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (let [splitted-result (cstring/split
                              result
                              #"; ")
            cookie-name-value (first
                                splitted-result)
            splitted-result (rest
                              splitted-result)
            [cookie-name
             cookie-value] (cstring/split
                             cookie-name-value
                             #"=")
            expires-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [expires-name
             expires-value] (cstring/split
                              expires-name-value
                              #"=")
            max-age-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [max-age-name
             max-age-value] (cstring/split
                              max-age-name-value
                              #"=")
            path-name-value (first
                              splitted-result)
            splitted-result (rest
                              splitted-result)
            [path-name
             path-value] (cstring/split
                           path-name-value
                           #"=")]
        
        (is
          (= cookie-name
             "session")
         )
        
        (is
          (= cookie-value
             "1234-uuid")
         )
        
        (is
          (= expires-name
             "Expires")
         )
        
        (let [simple-date-format (java.text.SimpleDateFormat.
                                   "EE, dd MMM yyyy HH:mm:ss zzz")]
          
          (is
            (< (- (.getTime
                    (.parse
                      simple-date-format
                      expires-value))
                  (.getTime
                    (java.util.Date.))
                )
               10000)
           )
          
         )
        
        (is
          (= max-age-name
             "Max-Age")
         )
        
        (is
          (= max-age-value
             "10")
         )
        
        (is
          (= path-name
             "Path")
         )
        
        (is
          (= path-value
             "/")
         )
        
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on true
          is-httponly-on nil
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (let [splitted-result (cstring/split
                              result
                              #"; ")
            cookie-name-value (first
                                splitted-result)
            splitted-result (rest
                              splitted-result)
            [cookie-name
             cookie-value] (cstring/split
                             cookie-name-value
                             #"=")
            expires-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [expires-name
             expires-value] (cstring/split
                              expires-name-value
                              #"=")
            max-age-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [max-age-name
             max-age-value] (cstring/split
                              max-age-name-value
                              #"=")
            path-name-value (first
                              splitted-result)
            splitted-result (rest
                              splitted-result)
            [path-name
             path-value] (cstring/split
                           path-name-value
                           #"=")
            secure-name-value (first
                                splitted-result)
            splitted-result (rest
                              splitted-result)
            [secure-name
             secure-value] (cstring/split
                             secure-name-value
                             #"=")]
        
        (is
          (= cookie-name
             "session")
         )
        
        (is
          (= cookie-value
             "1234-uuid")
         )
        
        (is
          (= expires-name
             "Expires")
         )
        
        (let [simple-date-format (java.text.SimpleDateFormat.
                                   "EE, dd MMM yyyy HH:mm:ss zzz")]
          
          (is
            (< (- (.getTime
                    (.parse
                      simple-date-format
                      expires-value))
                  (.getTime
                    (java.util.Date.))
                )
               10000)
           )
          
         )
        
        (is
          (= max-age-name
             "Max-Age")
         )
        
        (is
          (= max-age-value
             "10")
         )
        
        (is
          (= path-name
             "Path")
         )
        
        (is
          (= path-value
             "/")
         )
        
        (is
          (= secure-name
             "Secure")
         )
        
        (is
          (nil?
            secure-value)
         )
        
       )
      
     )
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on true
          is-httponly-on true
          is-persistent nil
          result (session-cookie-string
                   cookie-name
                   user-id
                   username
                   session-uuid
                   timeout-in-seconds
                   user-agent
                   is-secure-on
                   is-httponly-on
                   is-persistent)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (string?
          result)
       )
      
      (let [splitted-result (cstring/split
                              result
                              #"; ")
            cookie-name-value (first
                                splitted-result)
            splitted-result (rest
                              splitted-result)
            [cookie-name
             cookie-value] (cstring/split
                             cookie-name-value
                             #"=")
            expires-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [expires-name
             expires-value] (cstring/split
                              expires-name-value
                              #"=")
            max-age-name-value (first
                                 splitted-result)
            splitted-result (rest
                              splitted-result)
            [max-age-name
             max-age-value] (cstring/split
                              max-age-name-value
                              #"=")
            path-name-value (first
                              splitted-result)
            splitted-result (rest
                              splitted-result)
            [path-name
             path-value] (cstring/split
                           path-name-value
                           #"=")
            secure-name-value (first
                                splitted-result)
            splitted-result (rest
                              splitted-result)
            [secure-name
             secure-value] (cstring/split
                             secure-name-value
                             #"=")
            httponly-name-value (first
                                  splitted-result)
            splitted-result (rest
                              splitted-result)
            [httponly-name
             httponly-value] (cstring/split
                               httponly-name-value
                               #"=")]
        
        (is
          (= cookie-name
             "session")
         )
        
        (is
          (= cookie-value
             "1234-uuid")
         )
        
        (is
          (= expires-name
             "Expires")
         )
        
        (let [simple-date-format (java.text.SimpleDateFormat.
                                   "EE, dd MMM yyyy HH:mm:ss zzz")]
          
          (is
            (< (- (.getTime
                    (.parse
                      simple-date-format
                      expires-value))
                  (.getTime
                    (java.util.Date.))
                )
               10000)
           )
          
         )
        
        (is
          (= max-age-name
             "Max-Age")
         )
        
        (is
          (= max-age-value
             "10")
         )
        
        (is
          (= path-name
             "Path")
         )
        
        (is
          (= path-value
             "/")
         )
        
        (is
          (= secure-name
             "Secure")
         )
        
        (is
          (nil?
            secure-value)
         )
        
        (is
          (= httponly-name
             "HttpOnly")
         )
        
        (is
          (nil?
            httponly-value)
         )
        
       )
      
     )
    
    ;; Insert session
    
    (let [cookie-name "session"
          user-id nil
          username nil
          session-uuid "3-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          result (mongodb-find-one
                   cookie-name
                   {:uuid session-uuid})]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id "user-id"
          username nil
          session-uuid "2-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          result (mongodb-find-one
                   cookie-name
                   {:uuid session-uuid})]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id "user-id"
          username "username"
          session-uuid "1-uuid"
          timeout-in-seconds 10
          user-agent nil
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          result (mongodb-find-one
                   cookie-name
                   {:uuid session-uuid})]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id "user-id"
          username "username"
          session-uuid "5-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          result (mongodb-find-one
                   cookie-name
                   {:uuid session-uuid})]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (= user-id
           (:user-id result))
       )
      
      (is
        (= username
           (:username result))
       )
      
      (is
        (= user-agent
           (:user-agent result))
       )
      
      (is
        (= session-uuid
           (:uuid result))
       )
      
     )
    
    (Thread/sleep 2000)
    
    (let [cookie-name "session"
          user-id "user-id"
          username "username"
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          result (mongodb-find-one
                   cookie-name)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (< (- (.getTime
                (java.util.Date.))
              (.getTime
                (:created-at result))
            )
           5000)
       )
      
     )
    
   ))

(deftest test-get-cookie
  (testing "Test get cookie"
    
    (let [cookies nil
          cookie-name nil
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookies nil
          cookie-name :session
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookies "session=session-uuid; session-visible=exists"
          cookie-name nil
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookies "session=session-uuid; session-visible=exists"
          cookie-name ""
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [cookies "session=session-uuid; session-visible=exists"
          cookie-name :session
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (= result
           "session-uuid")
       )
      
     )
    
    (let [cookies "session=session-uuid; session-visible=exists"
          cookie-name :session-visible
          result (get-cookie
                   cookies
                   cookie-name)]
      
      (is
        (= result
           "exists")
       )
      
     )
    
   ))

(deftest test-get-accept-language
  (testing "Test get accept language"
    
    (let [request nil
          result (get-accept-language
                   request)]
      
      (is
        (= result
           "english")
       )
      
     )
    
    (let [request {}
          result (get-accept-language
                   request)]
      
      (is
        (= result
           "english")
       )
      
     )
    
    (let [request {:accept-language "sr,en;q=0.5"}
          result (get-accept-language
                   request)]
      
      (is
        (= result
           "serbian")
       )
      
     )
    
    (let [request {:accept-language "fr,en;q=0.5"}
          result (get-accept-language
                   request)]
      
      (is
        (= result
           "english")
       )
      
     )
    
   ))

(deftest test-get-session-obj
  (testing "Test get session obj"
    
    (let [request nil
          result (get-session-obj
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie "session=1234-uuid"}
          result (get-session-obj
                   request)]
      
      (is
        (not
          (nil?
            result))
       )
      
     )
    
    (let [request {:cookie "long-session=1234-uuid"}
          result (get-session-obj
                   request)]
      
      (is
        (not
          (nil?
            result))
       )
      
     )
    
   ))

(deftest test-get-preferences
  (testing "Test get preferences"
    
    (let [request nil
          result (get-preferences
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie "session=test-uuid"}
          result (get-preferences
                   request)]
      
      (let [user-db-obj (mongodb-find-one
	                         "user"
	                         {:username "test-user"})
	           _id (:_id user-db-obj)]
	       
	       (is
          (= (:user-id result)
             _id)
         )
	       
	       (is
          (= (:language result)
             "serbian")
         )
	       
	       (is
          (= (:language-name result)
             "Serbian")
         )
         
       )
      
     )
    
   ))

(deftest test-am-i-logged-in
  (testing "Test am i logged in"
    
    (let [request nil
          result (am-i-logged-in
                   request)]
      
      (is
        (= (:status result)
           (stc/unauthorized))
       )
      
      (is
        (= (get-in
             result
             [:headers
              (eh/content-type)])
           (mt/text-clojurescript))
       )
      
      (is
        (= (:body result)
           "It's not ok")
       )
      
     )
    
    (let [request {:cookie "session=6-uuid; session-visible=exists"
                   :accept-language "sr,en;q=0.5"}
          result (am-i-logged-in
                   request)]
      
      (is
        (= (:status result)
           (stc/unauthorized))
       )
      
      (is
        (= (get-in
             result
             [:headers
              (eh/content-type)])
           (mt/text-clojurescript))
       )
      
      (is
        (= (:body result)
           "It's not ok")
       )
      
     )
    
    (let [cookie-name "session"
          user-id "user-id"
          username "admin"
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          request {:cookie "session=1234-uuid; session-visible=exists"
                   :accept-language "sr,en;q=0.5"}
          result (am-i-logged-in
                   request)]
      
      (is
        (= (:status result)
           (stc/ok))
       )
      
      (is
        (= (get-in
             result
             [:headers
              (eh/content-type)])
           (mt/text-clojurescript))
       )
      
      (is
        (= (get-in
             result
             [:body
              :status])
           "It's ok")
       )
      
      (is
        (= (get-in
             result
             [:body
              :username])
           "admin")
       )
      
      (is
        (= (get-in
             result
             [:body
              :language])
           "serbian")
       )
      
      (is
        (= (get-in
             result
             [:body
              :language-name])
           "Serbian")
       )
      
     )
    
    (let [cookie-name "long-session"
          user-id "user-id"
          username "admin"
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          request {:cookie "long-session=1234-uuid; long-session-visible=exists"
                   :accept-language "sr,en;q=0.5"}
          result (am-i-logged-in
                   request)]
      
      (is
        (= (:status result)
           (stc/ok))
       )
      
      (is
        (= (get-in
             result
             [:headers
              (eh/content-type)])
           (mt/text-clojurescript))
       )
      
      (is
        (= (get-in
             result
             [:body
              :status])
           "It's ok")
       )
      
      (is
        (= (get-in
             result
             [:body
              :username])
           "admin")
       )
      
      (is
        (= (get-in
             result
             [:body
              :language])
           "serbian")
       )
      
      (is
        (= (get-in
             result
             [:body
              :language-name])
           "Serbian")
       )
      
     )
    
   ))

(deftest test-am-i-logged-in-fn
  (testing "Test am i logged in fn"
    
    (let [request nil
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [request {}
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (mongodb-delete-one
      "session"
      {:uuid "1234-uuid"})
    
    (let [request {:cookie "session=1234-uuid; session-visible=exists"}
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [cookie-name "session"
          user-id "user-id"
          username "username"
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          request {:cookie "session=1234-uuid; session-visible=exists"}
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (true?
          result)
       )
      
     )
    
    (let [request {:cookie "long-session=1-uuid; long-session-visible=exists"}
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [cookie-name "long-session"
          user-id "user-id"
          username "username"
          session-uuid "1234-uuid"
          timeout-in-seconds 10
          user-agent "user-agent"
          is-secure-on true
          is-httponly-on true
          is-persistent true
          session-string (session-cookie-string
                           cookie-name
                           user-id
                           username
                           session-uuid
                           timeout-in-seconds
                           user-agent
                           is-secure-on
                           is-httponly-on
                           is-persistent)
          request {:cookie "long-session=1234-uuid; long-session-visible=exists"}
          result (am-i-logged-in-fn
                   request)]
      
      (is
        (true?
          result)
       )
      
     )
    
   ))

(deftest test-refresh-session
  (testing "Test refresh session"
    
    (let [request nil
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {}
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie ""}
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:user-agent ""}
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie ""
                   :user-agent ""}
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie "session=1234-uuid"
                   :user-agent ""}
          result (refresh-session
                   request)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {:cookie "session=1234-uuid; session-visible=exists"
                   :user-agent "user-agent"}
          [result1
           result2] (refresh-session
                      request)]
      
      (is
        (number?
          (cstring/index-of
            result1
            "session=1234-uuid")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Max-Age=1800")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Path=/")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "session-visible=exists")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Max-Age=1800")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Path=/")
         )
       )
      
     )
    
    (let [request {:cookie "long-session=1234-uuid; long-session-visible=exists"
                   :user-agent "user-agent"}
          [result1
           result2] (refresh-session
                      request)]
      
      (is
        (number?
          (cstring/index-of
            result1
            "long-session=1234-uuid")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Max-Age=94608000")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Path=/")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "long-session-visible=exists")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Max-Age=94608000")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Path=/")
         )
       )
      
     )
    
   ))

(deftest test-session-cookie-string-fn
  (testing "Test session cookie string fn"
    
    (let [remember-me nil
          user nil
          uuid nil
          user-agent nil
          result (session-cookie-string-fn
                   remember-me
                   user
                   uuid
                   user-agent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [remember-me true
          user nil
          uuid nil
          user-agent nil
          result (session-cookie-string-fn
                   remember-me
                   user
                   uuid
                   user-agent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [remember-me true
          user {:_id "user-id"
                :username "username"}
          uuid nil
          user-agent nil
          result (session-cookie-string-fn
                   remember-me
                   user
                   uuid
                   user-agent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [remember-me true
          user {:_id "user-id"
                :username "username"}
          uuid "1234-uuid"
          user-agent nil
          result (session-cookie-string-fn
                   remember-me
                   user
                   uuid
                   user-agent)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [remember-me false
          user {:_id "user-id"
                :username "username"}
          uuid "1234-uuid"
          user-agent "user-agent"
          [result1
           result2] (session-cookie-string-fn
                      remember-me
                      user
                      uuid
                      user-agent)]
      
      (is
        (number?
          (cstring/index-of
            result1
            "session=1234-uuid")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Max-Age=1800")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Path=/")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Secure")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "HttpOnly")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "session-visible=exists")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Max-Age=1800")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Path=/")
         )
       )
      
     )
    
    (let [remember-me true
          user {:_id "user-id"
                :username "username"}
          uuid "1234-uuid"
          user-agent "user-agent"
          [result1
           result2] (session-cookie-string-fn
                      remember-me
                      user
                      uuid
                      user-agent)]
      
      (is
        (number?
          (cstring/index-of
            result1
            "long-session=1234-uuid")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Max-Age=94608000")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Path=/")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "Secure")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result1
            "HttpOnly")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "long-session-visible=exists")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Max-Age=94608000")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            result2
            "Path=/")
         )
       )
      
     )
    
   ))

(deftest test-delete-session-record
  (testing "Test delete session record"
    
    (let [request nil
          result (delete-session-record
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Cookies not present in request"})
       )
      
     )
    
    (let [request {}
          result (delete-session-record
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Cookies not present in request"})
       )
      
     )
    
    (let [request {:cookie "no-session=5678-uuid; no-session-visible=exists"}
          result (delete-session-record
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Session cookies not present in request"})
       )
      
     )
    
    (let [request {:cookie "session=1234-uuid; session-visible=exists"}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           response-body :body} (delete-session-record
                                  request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-plain))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "session=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "session-visible=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= response-body
           "Bye bye")
       )
      
     )
    
    (let [request {:cookie "long-session=1234-uuid; long-session-visible=exists"}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           response-body :body} (delete-session-record
                                  request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-plain))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= response-body
           "Bye bye")
       )
      
     )
    
   ))

(deftest test-create-indexes
  (testing "Test create indexes"
    
    (create-indexes)
    
    (is
      (mongodb-index-exists?
        "user"
        "username-unique-idx")
     )
    
    (is
      (mongodb-index-exists?
        "user"
        "email-unique-idx")
     )
    
    (is
      (mongodb-index-exists?
        "session"
        "short-session-idx")
     )
    
    (is
      (mongodb-index-exists?
        "session"
        "session-uuid-unique-idx")
     )
    
    (is
      (mongodb-index-exists?
        "long-session"
        "long-session-idx")
     )
    
    (is
      (mongodb-index-exists?
        "long-session"
        "long-session-uuid-unique-idx")
     )
    
   ))

(deftest test-get-pass-for-email-username
  (testing "Test get pass for email username"
    
    (let [email-username nil
          password nil
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "error"
             :password "error"}])
       )
      
     )
    
    (let [email-username ""
          password nil
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "error"
             :password "error"}])
       )
      
     )
    
    (let [email-username ""
          password ""
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "error"
             :password "error"}])
       )
      
     )
    
    (let [email-username "test-user-1"
          password ""
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "error"
             :password "error"}])
       )
      
     )
    
    (let [email-username "test-user-1"
          password "test-password"
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "error"
             :password "error"}])
       )
      
     )
    
    (mongodb-insert-one
      "user"
      {:username "test-user-1"
       :email "test@email.com"
       :password "test-password-1"})
    
    (let [email-username "test-user-1"
          password "test-password"
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "success"
             :password "error"}])
       )
      
     )
    
    (let [email-username "test-user-1"
          password "test-password-1"
          accept-language nil
          [result-status
           {result-username :username
            result-email :email
            result-password :password}] (get-pass-for-email-username
                                          email-username
                                          password
                                          accept-language)]
      
      (is
        (= result-status
           {:status "success"
            :email "success"
            :password "success"
            :username "test-user-1"
            :language "english"
            :language-name "English"})
       )
      
      (is
        (= result-username
           "test-user-1")
       )
      
      (is
        (= result-email
           "test@email.com")
       )
      
      (is
        (= result-password
           "test-password-1")
       )
      
     )
    
    (let [email-username "test@email.com"
          password "test-password"
          accept-language nil
          result (get-pass-for-email-username
                   email-username
                   password
                   accept-language)]
      
      (is
        (= result
           [{:status "error"
             :email "success"
             :password "error"}])
       )
      
     )
    
    (let [email-username "test@email.com"
          password "test-password-1"
          accept-language nil
          [result-status
           {result-username :username
            result-email :email
            result-password :password}] (get-pass-for-email-username
                                          email-username
                                          password
                                          accept-language)]
      
      (is
        (= result-status
           {:status "success"
            :email "success"
            :password "success"
            :username "test-user-1"
            :language "english"
            :language-name "English"})
       )
      
      (is
        (= result-username
           "test-user-1")
       )
      
      (is
        (= result-email
           "test@email.com")
       )
      
      (is
        (= result-password
           "test-password-1")
       )
      
     )
    
   ))

(deftest test-login-authentication
  (testing "Test login authentication"
    
    (let [request nil
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"}})
       )
      
     )
    
    (let [request {}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"}})
       )
      
     )
    
    (let [request {:user-agent "user-agent"}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"}})
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :body {}}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"}})
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :body {:email "not-existing-username"}}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"}})
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :body {:email "not-existing-username"
                          :password "not-existing-password"}}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/unauthorized)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"
                   :email "error"
                   :password "error"}})
       )
      
     )
    
    (mongodb-insert-one
      "user"
      {:username "test-user-1"
       :email "test@email.com"
       :password "test-password-1"})
    
    (let [request {:user-agent "user-agent"
                   :body {:email "test-user-1"
                          :password "test-password"}}
          result (login-authentication
                   request)]
      
      (is
        (= result
           {:status (stc/unauthorized)
            :headers {(eh/content-type) (mt/text-clojurescript)}
            :body {:status "error"
                   :email "success"
                   :password "error"}})
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :body {:email "test-user-1"
                          :password "test-password-1"}}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           {result-status :status
            result-email :email
            result-password :password
            result-username :username
            result-language :language
            result-language-name :language-name} :body} (login-authentication
                                                          request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "session="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=1800"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "session-visible="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=1800"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= result-status
           "success")
       )
      
      (is
        (= result-email
           "success")
       )
      
      (is
        (= result-password
           "success")
       )
      
      (is
        (= result-username
           "test-user-1")
       )
      
      (is
        (= result-language
           "english")
       )
      
      (is
        (= result-language-name
           "English")
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :body {:email "test-user-1"
                          :password "test-password-1"
                          :remember-me true}}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           {result-status :status
            result-email :email
            result-password :password
            result-username :username
            result-language :language
            result-language-name :language-name} :body} (login-authentication
                                                          request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= result-status
           "success")
       )
      
      (is
        (= result-email
           "success")
       )
      
      (is
        (= result-password
           "success")
       )
      
      (is
        (= result-username
           "test-user-1")
       )
      
      (is
        (= result-language
           "english")
       )
      
      (is
        (= result-language-name
           "English")
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :accept-language "sr,en;q=0.5"
                   :body {:email "test-user-1"
                          :password "test-password-1"
                          :remember-me true}}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           {result-status :status
            result-email :email
            result-password :password
            result-username :username
            result-language :language
            result-language-name :language-name} :body} (login-authentication
                                                          request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-clojurescript))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= result-status
           "success")
       )
      
      (is
        (= result-email
           "success")
       )
      
      (is
        (= result-password
           "success")
       )
      
      (is
        (= result-username
           "test-user-1")
       )
      
      (is
        (= result-language
           "serbian")
       )
      
      (is
        (= result-language-name
           "Serbian")
       )
      
     )
    
   ))

(deftest test-logout
  (testing "Test logout"
    
    (let [request nil
          result (logout
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Cookies not present in request"})
       )
      
     )
    
    (let [request {}
          result (logout
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Cookies not present in request"})
       )
      
     )
    
    (let [request {:cookie "no-session=5678-uuid; no-session-visible=exists"}
          result (logout
                   request)]
      
      (is
        (= result
           {:status (stc/internal-server-error)
            :headers {(eh/content-type) (mt/text-plain)}
            :body "Session cookies not present in request"})
       )
      
     )
    
    (let [request {:cookie "session=4321-uuid; session-visible=exists"}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           response-body :body} (logout
                                  request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-plain))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "session=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "session-visible=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= response-body
           "Bye bye")
       )
      
     )
    
    (let [request {:cookie "long-session=4321-uuid; long-session-visible=exists"}
          {status :status
           {content-type (eh/content-type)
            [cookie1
             cookie2] (rsh/set-cookie)} :headers
           response-body :body} (logout
                                  request)]
      
      (is
        (= status
           (stc/ok))
       )
      
      (is
        (= content-type
           (mt/text-plain))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible=destroyed"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=0"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (= response-body
           "Bye bye")
       )
      
     )
    
   ))

(deftest test-set-session-cookies
  (testing "Test set session cookies"
    
    (let [request nil
          response nil
          result (set-session-cookies
                   request
                   response)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {}
          response nil
          result (set-session-cookies
                   request
                   response)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [request {}
          response {}
          result (set-session-cookies
                   request
                   response)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [request {:user-agent "user-agent"}
          response {}
          result (set-session-cookies
                   request
                   response)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "not-existing-session=5678-uuid; not-existing-session-visible=exists"}
          response {}
          result (set-session-cookies
                   request
                   response)]
      
      (is
        (not
          (nil?
            result))
       )
      
      (is
        (map?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "session=1234-uuid; session-visible=exists"}
          response {}
          {{[cookie1
             cookie2] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=1800"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=1800"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "long-session=1234-uuid; long-session-visible=exists"}
          response {}
          {{[cookie1
             cookie2] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "long-session=1234-uuid; long-session-visible=exists"}
          response {:headers {(rsh/set-cookie) ""}}
          {{[cookie1
             cookie2] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "long-session=1234-uuid; long-session-visible=exists"}
          response {:headers {(rsh/set-cookie) "test-cookie-name=test-cooki-value"}}
          {{[cookie1
             cookie2
             cookie3] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "test-cookie-name=test-cooki-value")
         )
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "long-session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Path=/"))
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "long-session=1234-uuid; long-session-visible=exists"}
          response {:headers {(rsh/set-cookie) true}}
          {{[cookie1
             cookie2] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "long-session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "long-session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "Path=/"))
       )
      
     )
    
    (let [request {:user-agent "user-agent"
                   :cookie "long-session=1234-uuid; long-session-visible=exists"}
          response {:headers {(rsh/set-cookie) ["test-cookie-name1=test-cookie-value1"
                                                "test-cookie-name2=test-cookie-value2"]}}
          {{[cookie1
             cookie2
             cookie3
             cookie4] (rsh/set-cookie)} :headers} (set-session-cookies
                                                    request
                                                    response)]
      
      (is
        (number?
          (cstring/index-of
            cookie1
            "test-cookie-name1=test-cookie-value1"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie2
            "test-cookie-name2=test-cookie-value2"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "long-session=1234-uuid"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Path=/"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "Secure"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie3
            "HttpOnly"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie4
            "long-session-visible=exists"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie4
            "Expires="))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie4
            "Max-Age=94608000"))
       )
      
      (is
        (number?
          (cstring/index-of
            cookie4
            "Path=/"))
       )
      
     )
    
   ))

