#!/usr/bin/env clojure -M

(ns aoc24-01
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn read-columns [filename]
  (let [lines (split (read-input filename) #"\n")
     col1 (sort (mapv #(Integer/parseInt(trim (first (split % #"\s+")))) lines))
     col2 (sort (mapv #(Integer/parseInt(trim (second (split % #"\s+")))) lines))]
     [col1 col2]
  )
)

(defn distance [col1 col2]
  (reduce + (map (fn [x y] (abs (- x y))) col1 col2))
)

(defn -main [& args]
  ; Part a
  (let [[col1 col2] (read-columns (first args))]
    (println "The answer to part a is " (distance col1 col2))

  ; Part b
    (let [colb (reduce (fn [map v] (update map v (fnil inc 0))) {} col2)]
      (println "The answer to part b is " 
        (reduce + (map (fn [l] (* l (or(colb l) 0))) col1))
      )
    )
  )
)

;; Call the main function
(apply -main *command-line-args*)

