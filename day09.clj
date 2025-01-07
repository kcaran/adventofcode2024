#!/usr/bin/env -S clojure -M
;
; part a - I was off by one number in the main run because the end had
; empty space I needed to account for
;

(ns day09
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn pop-val [lmap pos]
  (loop [head 0]
 ;  (println pos tail (lmap tail) (>= 0 ((lmap tail) 0))) 
    (if (> (+ ((lmap head) 1) ((lmap head) 2)) pos)
      ((lmap head) 0)
      (recur (inc head))
    )
  )
)

(defn pop-end [lmap pos]
  (loop [pos pos tail (dec (count lmap))]
 ;  (println pos tail (lmap tail) (>= 0 ((lmap tail) 0))) 
    (if (<= ((lmap tail) 1) pos)
      (if (>= ((lmap tail) 0) 0) 
        [ (dec pos) ((lmap tail) 0) ]
        (recur (dec ((lmap tail) 1)) (dec tail))
      )
      (recur pos (dec tail))
    )
  )
)

(defn checksum [arg]
  (let [lmap (arg 0) tail (dec (arg 1))]
  (loop [pos 0 chksum 0 tail tail]
;   (println chksum pos tail)
    (if (> pos tail)
      chksum
      (if (< (pop-val lmap pos) 0)
         (let [end (pop-end lmap tail)]
;          (println "KAC" end)
           ; Have to make sure there isn't space at the end!
           (recur (inc pos) (if (> (end 0) pos) (+ chksum (* (end 1) pos)) chksum) (end 0))
         )
         (recur (inc pos) (+ chksum (* (pop-val lmap pos) pos)) tail)
      )
    )
  )
  )
)

(defn parse [str]
  (reduce (fn [[lmap pos] [i block]]
    (if (zero? block)
    [lmap pos]
    [(conj lmap [
       (if (even? i) (/ i 2) -1)
       pos
       block
      ])
     (+ pos block)]))
    [[] 0]
    (map-indexed (fn [i b] [i (Integer. b)]) (clojure.string/split (clojure.string/trim-newline str) #""))
  )
)

(defn -main [& args]
  (let [lmap (parse (myutils/read-input (first args)))]
    (println "The block checksum is " (checksum lmap))
  )
)

;; Call the main function
(apply -main *command-line-args*)

