#!/usr/bin/env -S clojure -M

; part b - How do you define a loop? For part b the direction has to the same
; as well as the position.

(ns day06
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn up [lmap pos]
  (if (zero? (pos 0))
    nil
    (if (= (get-in lmap [(dec (pos 0)) (pos 1)]) "#")
       [(pos 0) (pos 1) ">"]
       [(dec (pos 0)) (pos 1) "^"]
    )
  )
)

(defn down [lmap pos]
  (if (= (pos 0) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(inc (pos 0)) (pos 1)]) "#")
       [(pos 0) (pos 1) "<"]
       [(inc (pos 0)) (pos 1) "v"]
    )
  )
)

(defn right [lmap pos]
  (if (= (pos 1) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(pos 0) (inc (pos 1))]) "#")
       [(pos 0) (pos 1) "v"]
       [(pos 0) (inc (pos 1)) ">"]
    )
  )
)

(defn left [lmap pos]
  (if (zero? (pos 1))
    nil
    (if (= (get-in lmap [(pos 0) (dec (pos 1))]) "#")
       [(pos 0) (pos 1) "^"]
       [(pos 0) (dec (pos 1)) "<"]
    )
  )
)

(def move {
   "^" up
   "v" down
   ">" right
   "<" left
  }
)

(defn parse [file]
  (mapv (fn [x] (clojure.string/split x #"")) (clojure.string/split (myutils/read-input file) #"\n"))
)

(defn start [lmap]
  (for [y (range (count lmap))
        x (range (count (first lmap)))
        :let [point (get-in lmap [y x])]
        :when (= point "^")]
    [y x "^"]
  )
)

(defn visit [pos]
  (str (pos 0) "," (pos 1))
)

; Having issues with start returning a list of vectors and not a single vector
(defn traverse [lmap]
  (loop [pos (vec( apply concat (start lmap))) visited #{ (visit pos) } ]
    (if (nil? pos)
      visited
      (do
;       (println pos)
        (recur ((move (pos 2)) lmap pos) (conj visited (visit pos)))
      )
    )
  )
)

(defn visitb [pos]
  (str (pos 0) "," (pos 1) "," (pos 2) )
)

; Having issues with start returning a list of vectors and not a single vector
(defn traverseb [lmap]
  (loop [pos (vec( apply concat (start lmap))) visited #{} ]
    (if (nil? pos)
      0
      (do
;       (println pos)
        (if (visited (visitb pos))
          1
          (recur ((move (pos 2)) lmap pos) (conj visited (visitb pos)))
        )
      )
    )
  )
)

(defn partb [lmap y x]
  (if (= (get-in lmap [y x]) ".")
    (do
;     (println y x)
      (traverseb (assoc-in lmap [y x] "#") )
    )
    0
  )
)

(defn -main [& args]
  (let [lmap (parse (first args))]
  ; Part a
  (println "The positions visited is "
    (count (traverse lmap))
  )

  ; Part b
  (println "The number of loops created is "
    (reduce
      (fn [acc y]
        (reduce
          (fn [inner-acc x] (+ inner-acc (partb lmap y x)))
          acc
          (range (count (first lmap)))
        )
      )
      0
      (range (count lmap))
    )
  )

  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

