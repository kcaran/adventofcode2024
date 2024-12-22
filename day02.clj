#!/usr/bin/env -S clojure -M

(ns aoc24-02
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

;
; I originally had a count of the bad levels, but of course a level can be bad in
; multiple ways! Need to hash the bad levels and count them instead.
;
(defn is_safe [report threshold]
  (let [levels (map read-string (clojure.string/split report #"\s+"))
        diffs (partition 2 1 levels)
        cmpp (if (apply < (first diffs)) < >)
        not_safe (set (concat
          (filter (fn [x] (= (apply - x) 0)) diffs)
          (filter (fn [x] (not (apply cmpp x))) diffs)
          (filter (fn [x] (> (abs (apply - x)) 3)) diffs)
        ))
   ]
;(pr diffs)

;(println "This report is " not_safe)
   (if (> (count not_safe) threshold) 0 1)
  )
)

(defn -main [& args]
  ; Part a
  (let [reports (split (read-input (first args)) #"\n")]
    (println "The number of safe reports is "
      (reduce + (map (fn [x] (is_safe x 0)) reports))
 ;    (reduce + (map (fn [l] ((or (is_safe l) 0)) reports)))
    )
    (println "The number of reports with less than two unsafe levels is "
      (reduce + (map (fn [x] (is_safe x 1)) reports))
    )
  )
)

;; Call the main function
(apply -main *command-line-args*)

