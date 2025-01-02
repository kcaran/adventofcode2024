#!/usr/bin/env -S clojure -M

; I'm having trouble with the differenc between (map ...) and (vec map ..). It
; seems like I'm assuming that the output of the map function should be a
; vector by default? Because of course that is the way I want to use it?
;

(ns day04
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(def word ["X" "M" "A" "S"])

(def dirs [
	(fn [y x] [(- y 1) (- x 1)] ) ; 10:30
	(fn [y x] [(- y 1) (+ x 1)] ) ; 1:30
	(fn [y x] [(+ y 1) (+ x 1)] ) ; 4:30
	(fn [y x] [(+ y 1) (- x 1)] ) ; 7:30
	(fn [y x] [(- y 1) x] ) ; up
	(fn [y x] [(+ y 1) x] ) ; down
	(fn [y x] [y (- x 1)] ) ; left
	(fn [y x] [y (+ x 1)] ) ; right
])

(defn diaglet [wfind y x letter]
  (if (or
    (and
      (= (get-in wfind ((dirs 0) y x)) letter)
      (= (get-in wfind ((dirs 2) y x)) letter)
    )
    (and
      (= (get-in wfind ((dirs 1) y x)) letter)
      (= (get-in wfind ((dirs 3) y x)) letter)
    )
  ) 0
    (reduce +
      (map
        (fn [i] (if (= (get-in wfind ((dirs i) y x)) letter) 1 0)) (range 4)
      )
    )
  )
)

(defn findmas [wfind y x]
  (if (and
    (= ((wfind y) x) "A")
    (= (diaglet wfind y x "M") 2)
    (= (diaglet wfind y x "S") 2)
  ) 1 0)
)

(defn findlet [wfind y x level dir]
;(println 'KAC y x level)
  (if (and
    (>= y 0)
    (>= x 0)
    (< y (count wfind))
    (< x (count wfind))
    (= ((wfind y) x) (word level))
    )
    (cond
      (= level 3) (do (println x y ((wfind y) x)) 1)
      (= level 0)
        (reduce +
          (map
            (fn [i]
              (apply findlet (concat [wfind] ((dirs i) y x) [(+ level 1) i]))
            )
            (range (count dirs))
          )
        )
       :else (apply findlet (concat [wfind] ((dirs dir) y x) [(+ level 1) dir]))
      )
    (do (println "Not applicable") 0)
  )
)

(defn wfind [file]
  (vec (map (fn [x] (clojure.string/split x #"")) (clojure.string/split (myutils/read-input file) #"\n")))
)

(defn parta [words]
  (reduce +
    (mapcat
      (fn [y] 
        (map (fn [x] (findlet words y x 0 0)) (range (count words)))
      ) (range (count words))
    )
  )
)

(defn partb [words]
  (reduce +
    (mapcat
      (fn [y] 
        (map (fn [x] (findmas words y x)) (range (count words)))
      ) (range (count words))
    )
  )
)

(defn -main [& args]
  (let [words (wfind (first args))]
  ; Part a
  (println "The number of XMASes found is " (parta words))
  (println "The number of MASes in an X found is " (partb words))
  )
)

;; Call the main function
(apply -main *command-line-args*)

