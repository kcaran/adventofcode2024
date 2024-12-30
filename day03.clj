#!/usr/bin/env -S clojure -M

(ns day03
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn parta [code]
  (println "KAC" code "\n")
  (reduce +
    (map
      (fn [n] (* (read-string (n 1)) (read-string (n 2))))
      (re-seq #"mul\((\d+),(\d+)\)" code)
    )
  )
)

;; In clojure, the sm flags are at the beginning of the regex
(defn partb [code]
  (let [codeseq (re-seq #"(?sm)(?:(.*?)(?:(do(?:n't)?\(\))|\z))" code)]
  (reduce +
    (parta (((vec codeseq) 0) 1))
    (map
      (fn [n]
        (if (not= (((vec codeseq) n) 2) "don't()")
              (parta (((vec codeseq) (+ n 1)) 1)) 0)
      )
      (range (- (count codeseq) 1))
    )
  ))
)

(defn -main [& args]
  ; Part a
  (let [code (read-input (first args))]
    (println "The sum of all multiplications is " (parta code))
  )
  (let [code (read-input (first args))]
    (println "The do/don't sum of all multiplications is " (partb code))
  )
)

;; Call the main function
(apply -main *command-line-args*)

