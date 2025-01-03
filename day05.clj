#!/usr/bin/env -S clojure -M

; I'm having trouble with the differenc between (map ...) and (vec map ..). It
; seems like I'm assuming that the output of the map function should be a
; vector by default? Because of course that is the way I want to use it?
;

(ns day05
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn parse [file]
  (apply (fn [order updates] [(set (clojure.string/split order #"\n")) (vec (clojure.string/split updates #"\n"))]) (clojure.string/split (myutils/read-input file) #"\n\n"))
)

(defn pages [upd]
  (vec (reverse (clojure.string/split upd #",")))
)

(defn legal-update [pages rules]
  (if (= (reduce +
    (map (fn [r] (if (rules r) 1 0))
      (flatten (map (fn [i] (map (fn [j] (str (pages i) "|" (pages j))) (range (+ i 1) (count pages)))) (range 0 (- (count pages) 1))))
    )
  ) 0)
    (Integer. (pages (/ (- (count pages) 1) 2)))
    0
  )
)

(defn fix-update [pages rules]
 (loop [p pages i 0]              ; loop over pages starting with first
   (if (>= i (dec (count p)))     ; if only one page left
     (do
     (legal-update p rules)
     )
     (recur                       ; try again with new set of pages
       (loop [pi p j (inc i)]     ; checking against next page
         (if (>= j (count pi))    ; no pages left
           pi
           (recur
             (if (rules (str (pi i) "|" (pi j))) ; if these need to be swapped
               (assoc (assoc pi i (nth pi j)) j (nth pi i))
               pi
             )
             (inc j)
           )
         )
       )
       (inc i)
     )
   )
  )
)

(defn -main [& args]
  (let [[rules updates] (parse (first args))]
  ; Part a
  (println "The total for the legal updates is "
    (reduce +
      (map (fn [u] (legal-update (pages u) rules)) updates)
    )
  )

  ; Part b
  (println "The total for the corrected updates is "
    (reduce +
      (map (fn [u] (if (= 0 (legal-update (pages u) rules)) (fix-update (pages u) rules) 0))
        updates)
    )
  )
  )
)

;; Call the main function
(apply -main *command-line-args*)

