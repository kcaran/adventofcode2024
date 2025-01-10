#!/usr/bin/env -S clojure -M

(ns day13
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

;(defn move [val but]
;  (quot [val 0
;)

(defn testbut [mach a mins]
  (let [diffax (- ((mach 2) 0) (* ((mach 0) 0) a))
        diffay (- ((mach 2) 1) (* ((mach 0) 1) a))
        quotbx (quot diffax ((mach 1) 0))
        quotby (quot diffay ((mach 1) 1))
        rembx (rem diffax ((mach 1) 0))
        remby (rem diffay ((mach 1) 1))
       ]
;(println a quotbx)
   [ (if (and (zero? rembx)
           (zero? remby)
           (= quotbx quotby)
           (< quotbx 100)
      )
      (min mins (+ (* a 3) quotbx))
      mins
      )
      (max quotbx quotby)
   ]
  )
)

(defn prize [mach]
  (loop [mins 400 trya (min 100 (quot ((mach 2) 0) ((mach 0) 0)) (quot ((mach 2) 1) ((mach 0) 1)))]
    (let [score (testbut mach trya mins)]
      (if (or (zero? trya) (> (score 1) 100))
         (score 0)
         (recur (score 0) (dec trya))
      )
    )
  )
)


(defn parse [str]
  (let [result (vec (map (fn [x] (vec (re-seq #"(?:.*?)X[+=](\d+)(?:.*?)Y[+=](\d+)" x))) (clojure.string/split str #"\n\n")))]
    (vec (map (fn [x] (vec (map (fn [m] [ (Integer. (m 1)) (Integer. (m 2)) ]) x))) result))
  )
)

(defn -main [& args]
  (let [lmap (parse (myutils/read-input (first args)))]
    (println "The minimum tokens is "
      (reduce +
        (map (fn [s]
          (let [p (prize s)]
            (if (< p 400) p 0)
          )) lmap
        )
      )
    )
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

