#!/usr/bin/env -S clojure -M
;
; part b - Challenging! Went to Reddit but they were actually no help! I
; was finally able to create my own test case (29702) because I wasn't
; moving two consecutive files to the same place correctly
;

(ns day09
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn pop-val [lmap pos]
  (loop [head 0]
    (if (> (+ ((lmap head) 1) ((lmap head) 2)) pos)
      (if (>= pos ((lmap head) 1)) ((lmap head) 0) -1)
      (recur (inc head))
    )
  )
)

(defn move [lmap pos tail]
; (println "KAC moving" (lmap pos) (lmap tail))
  (loop [inmap [] p 0 moved 0]
    (if (> p (dec (count lmap)))
      inmap
      (let [c (lmap p) t (lmap tail)]
      (cond
        (and (= p pos) (= (c 2) (t 2)))
          (recur (conj inmap [(t 0) (c 1) (t 2)]) (inc p) 1)
        (and (= p pos) (> (c 2) (t 2)))
          (recur (conj inmap [(t 0) (c 1) (t 2)]
            [-1 (+ (c 1) (t 2)) (- (c 2) (t 2))] ) (inc p) 1)
        (and (= p tail) (> moved 0))
          (recur inmap (inc p) moved) 
        :else (recur (conj inmap c) (inc p) moved)
      ))
    )
  )
)

(defn defrag [lmap]
  (loop [newmap lmap pos 0 tail (dec (count lmap))]
; (if (= pos 0) (println "Moving" tail newmap))
    (if (< tail 0)
      newmap
      (cond
        (< ((newmap tail) 0) 0)
          (recur newmap 0 (dec tail))
        (< pos tail)
          ; If there is an empty space that will fit
          (if (and (< ((newmap pos) 0) 0) (>= ((newmap pos) 2) ((newmap tail) 2)))
            (let [m (move newmap pos tail)] (recur m 0 (dec (count m))))
            (recur newmap (inc pos) tail)
          )
        :else
          (recur newmap 0 (dec tail))
      )
    )
  )
)

;
; For part b, do the defrag first
;
(defn printfile [lm]
  (apply str (map (fn [pos] (let [val (day09/pop-val lm pos)] (if (> val 0) val 'x))) (range (+ ((last lm) 1) ((last lm) 2)))))
)

(defn checksum [lmap]
  (reduce (fn [acc val]
    (+ acc val))
    (bigdec 0)
    (map (fn [pos] (let [val (pop-val lmap pos)]
      (if (> val 0) (println pos val))
      (if (> val 0) (* val pos) 0)))
      (range (+ ((last lmap) 1) ((last lmap) 2)))
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
  (let [lmap ((parse (myutils/read-input (first args))) 0)]
    (println "The block checksum is " (checksum (defrag lmap)))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

