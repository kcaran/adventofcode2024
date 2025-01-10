#!/usr/bin/env -S clojure -M
;
; day11.clj $(cat test11.txt)
;

(ns day11
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn solve [stone blinks]
(println stone blinks)
  (let [ss (str stone) len (count ss)]
  (cond
    (zero? blinks) 1
    ; in memory
    (zero? stone) (solve 1 (dec blinks))
    (even? len)
      (+ (solve (Integer. (subs ss 0 (/ len 2))) (dec blinks))
         (solve (Integer. (subs ss (/ len 2))) (dec blinks))
      )
    :else (solve (* stone 2024) (dec blinks))
  )
  )
)

(defn -main [& args]
  (println "The number of stones is "
    (reduce + (map (fn [x] (solve (Integer. x) 25)) args))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

