#!/usr/bin/env -S clojure -M
;
; Part a - I've been saving 
;

(ns day10
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn up [lmap pos]
  (if (zero? (pos 0))
    nil
    (if (= (get-in lmap [(dec (pos 0)) (pos 1)]) (inc (get-in lmap pos)))
       [(dec (pos 0)) (pos 1)]
       nil
    )
  )
)

(defn down [lmap pos]
  (if (= (pos 0) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(inc (pos 0)) (pos 1)]) (inc (get-in lmap pos)))
       [(inc (pos 0)) (pos 1)]
       nil
    )
  )
)

(defn right [lmap pos]
  (if (= (pos 1) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(pos 0) (inc (pos 1))]) (inc (get-in lmap pos)))
       [(pos 0) (inc (pos 1))]
       nil
    )
  )
)

(defn left [lmap pos]
  (if (zero? (pos 1))
    nil
    (if (= (get-in lmap [(pos 0) (dec (pos 1))]) (inc (get-in lmap pos)))
       [(pos 0) (dec (pos 1))]
       nil
    )
  )
)

(def move [
   up
   down
   right
   left
  ]
)

(defn height [lmap pos]
  (get-in lmap (vec (map (fn [x] (Integer. x)) (clojure.string/split pos #","))))
)

(defn visit [pos]
  (str (pos 0) "," (pos 1))
)

(defn traverse [lmap start]
  (loop [moves [ start ] visited #{} ]
    ; If there are no moves left
    (if (nil? moves)
      (filter (fn [x] (= (height lmap x) 9)) visited)
      (let [pos (first moves)]
        (if (nil? (visited (visit pos)))
          (recur
            (into (next moves) (remove nil? (map (fn [m] ((move m) lmap pos)) (range 4))))
            (conj visited (visit pos))
          )
          (recur (next moves) visited)
        )
      )
    )
  )
)

(defn theads [lmap]
  (into #{}
    (for [y (range (count lmap))
          x (range (count (first lmap)))
          :when (= ((lmap y) x) 0)]
      [y x]
    )
  )
)

(defn trailhead [lmap]
  (reduce +
    (map (fn [h] (let [tops (traverse lmap h)] (println h tops) (count tops))) (theads lmap))
  )
)

(defn parse [str]
  (vec
    (map (fn [y] (vec (map (fn [x] (Integer. x)) (clojure.string/split y #"")))) (clojure.string/split str #"\n"))
  )
)

(defn -main [& args]
  (let [lmap (parse (myutils/read-input (first args)))]
    (println "The trailhead score is " (trailhead lmap))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

