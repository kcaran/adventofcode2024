#!/usr/bin/env -S clojure -M

(ns aoc24-02
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

;
; I originally had a count of the bad levels, but of course a level can be bad in
; multiple ways! Need to hash the bad levels and count them instead.
;
; That isn't right either - I need to *remove* levels and test again
;
(defn is_safe [levels]
  (let [diffs (partition 2 1 levels)
        cmpp (if (apply < (first diffs)) < >)
        not_safe (set (concat
          (filter (fn [x] (= (apply - x) 0)) diffs)
          (filter (fn [x] (not (apply cmpp x))) diffs)
          (filter (fn [x] (> (abs (apply - x)) 3)) diffs)
        ))
   ]
;  (pr diffs)

;  (println "This report is " not_safe)
   (if (= (count not_safe) 0) true false)
  )
)

(defn check_levels [report threshold]
  (let [levels (map read-string (clojure.string/split report #"\s+"))
   ]
   (if (= threshold 0)
     (is_safe levels)
     (reduce (fn [acc x] (or acc x)) false
       ; Iterate through the levels, taking one away at each step
       (map (fn [n] (is_safe (concat (take n levels) (drop (inc n) levels))))
         (range (count levels))
       )
     )
   )
 )
)

(defn -main [& args]
  ; Part a
  (let [reports (split (read-input (first args)) #"\n")]
    (println "The number of safe reports is "
      ; OR a bunch of boolean results together
      (reduce + (map (fn [x] (if (check_levels x 0) 1 0)) reports))
    )
    (println "The number of reports with less than two unsafe levels is "
      (reduce + (map (fn [x] (if (check_levels x 1) 1 0)) reports))
    )
  )
)

;; Call the main function
(apply -main *command-line-args*)

