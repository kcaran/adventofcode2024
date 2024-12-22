#!/usr/bin/env clojure -M

(ns myutils
(:require [clojure.java.io :refer [reader]])
(:require [clojure.string :refer [split trim join]])
)

(defn exec-command [command]
  (try
    (let [process (.exec (Runtime/getRuntime) command)
       output (with-open [reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream process)))]
         (join "\n" (line-seq reader)))
       error (with-open [err-reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getErrorStream process)))]
          (join "\n" (line-seq err-reader)))
       exit-code (.waitFor process)]
      (if (zero? exit-code)
        output
        (throw (Exception. (str "Error executing command: " error))))
     )
    (catch Exception e (str "Caught exception: " (.getMessage e)))
  )
)

(defn read-input [file]
  (if (re-matches #".*\.7z$" file)
    (exec-command (str "7zz e -so -p" (exec-command "cat .password") " " file))
    (slurp file)
  )
)
