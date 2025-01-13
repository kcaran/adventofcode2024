#!/usr/bin/env -S clojure -M
;
; Part a: Had an off-by-one error in calculating the middle of the grid,
; since everything is zero-based.
;
; Part b: Piped output to less and searched for a row of 10 asterisks
;

(ns day14
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn move [lmap pt times]
  (let [py ((pt 0) 0) px ((pt 0) 1) vy ((pt 1) 0) vx ((pt 1) 1) maxy ((lmap 1) 0) maxx ((lmap 1) 1)]
    [ [ (mod (+ py (* times vy)) maxy ) (mod (+ px (* times vx)) maxx ) ] [ vy vx ] ]
  )
)

(defn score [pt m]
  (let [py ((pt 0) 0) px ((pt 0) 1) hy (/ (dec (m 0)) 2) hx (/ (dec (m 1)) 2)]
;(println py px)
    (cond
      (or (= py hy) (= px hx)) 0
      (and (< py hy) (< px hx)) 1
      (and (< py hy) (> px hx)) 2
      (and (> py hy) (< px hx)) 3
      (and (> py hy) (> px hx)) 4
    )
  )
)

(defn go [lmap times]
   (map (fn [p] (move lmap p times)) (lmap 0))
)

(defn safety [lmap run]
  (let [moved (go lmap run)]
    (reduce (fn [m pt]
       (let [s (score pt (lmap 1))]
          (if (> s 0) (update m s (fnil inc 0)) m)
        ))
       {}
    moved))
)

(defn prmap [lmap]
  (let [mp (vec (repeat ((lmap 1) 0) (vec (repeat ((lmap 1) 1) '.))))]
    (run! (fn [l] (println l))
      (reduce (fn [mpp pt] (assoc-in mpp pt '*))
        mp
        (map (fn [p] (p 0)) (lmap 0))
      )
    )
  )
)

(defn parse [str]
  (let [result (vec (map (fn [x] (vec (flatten (re-seq #"p\=(-?\d+),(-?\d+) v\=(-?\d+),(-?\d+)" x)))) (clojure.string/split str #"\n")))]
    (loop [cnt 0 lmap [] maxy 0 maxx 0] 
      (if (>= cnt (count result))
        [ lmap [ (inc maxy) (inc maxx) ] ]
        (let [px (Integer. ((result cnt) 1)) py (Integer. ((result cnt) 2))
              vx (Integer. ((result cnt) 3)) vy (Integer. ((result cnt) 4))]
          (recur (inc cnt) (conj lmap [[py px] [vy vx]]) (max maxy py) (max maxx px))
        )
      )
    )
  )
)

(defn -main [& args]
  (let [lmap (parse (myutils/read-input (first args)))
     safe (reduce * (vals (safety lmap 100))) ]
    (println "The safety score is " safe)
    (loop [mp lmap cnt 0]
      (do
        (println "\nThe count is" cnt)
        (prmap mp)
      )
      (if (<= cnt (* ((lmap 1) 0) ((lmap 1) 1)))
        (recur [(vec (go mp 1)) (lmap 1)] (inc cnt))
      )
    )
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

