#!/usr/bin/env -S clojure -M
;
; day11.clj $(cat test11.txt)
;
; I did get a little help from Reddit, but still had to create the memoization
;

(ns day11
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(def memo-cache (atom {}))


(defn calc-solve [stone blinks]
  (declare solve)
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

(defn solve [stone blinks]
  (if-let [result (get @memo-cache [stone blinks])]
    result
    (let [result (calc-solve stone blinks)]
      (swap! memo-cache assoc [stone blinks] result)
      result
    ) 
  )
)

(defn -main [& args]
  (println "The number of stones for part a is "
    (reduce + (map (fn [x] (solve (Integer. x) 25)) args))
  )
  (println "The number of stones for part b is "
    (reduce + (map (fn [x] (solve (Integer. x) 75)) args))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

