#!/usr/bin/env -S clojure -M

(ns day08
(:require [clojure.string :refer [split trim]])
(:require [clojure.set :as set])
(:require [myutils :refer [read-input]])
)

(defn in-range [pt maxi]
  (and
    (>= (pt 0) 0)
    (>= (pt 1) 0)
    (< (pt 0) maxi)
    (< (pt 1) maxi)
  )
)

(defn comp-anti [nodes p1 p2 maxi]
  (reduce (fn [acc pt]
 ;  (println "comp-antib" p1 p2 pt maxi)
    (if (in-range pt maxi)
      (conj acc pt)
      acc
      )
    )
    nodes
    [
      [ (- (* 2 (p1 0)) (p2 0)) (- (* 2 (p1 1)) (p2 1)) ]
      [ (- (* 2 (p2 0)) (p1 0)) (- (* 2 (p2 1)) (p1 1)) ]
    ]
  )
)

(defn new-anti [pt diff]
  [ (+ (pt 0) (diff 0)) (+ (pt 1) (diff 1)) ]
)

(defn comp-antib [nodes p1 p2 maxi]
  (let [diff [ [ (- (p1 0) (p2 0)) (- (p1 1) (p2 1)) ] [ (- (p2 0) (p1 0)) (- (p2 1) (p1 1)) ] ] ]
  (loop [pt1 p1 pt2 p2 nodes nodes]
  (if (and (not (in-range pt1 maxi)) (not (in-range pt2 maxi)))
    nodes
    (recur (new-anti pt1 (diff 0)) (new-anti pt2 (diff 1))
      (reduce (fn [acc pt]
 ;  (println "comp-antib" p1 p2 pt maxi)
        (if (in-range pt maxi)
          (conj acc pt)
          acc
        )
        )
        nodes
        [pt1 pt2]
      )
    )
    )
  )
  )
)

(defn antinodes [points maxi partb]
  (let [compute (if (nil? partb) comp-anti comp-antib)]
  (loop [ant (first points) others (next points) nodes #{}]
    (if (nil? others)
      nodes
      (recur (first others) (next others)
        (loop [nant (first others) inner-o (next others) inner-n nodes]
;         (println ant nant inner-o)
          (if (nil? inner-o)
            (compute inner-n ant nant maxi)
            (recur (first inner-o) (next inner-o) (compute inner-n ant nant maxi))
          )
        )
      )
    )
  )
  )
)

; Relied on copilot to convert to a hash of vectors and usage of :let and :when
(defn read-map [lmap]
  (reduce
    (fn [acc [y x p]] (update acc p (fnil conj []) [y x]))
    {}
    (for [y (range (count lmap))
      x (range (count (first lmap))) 
      :let [point (get-in lmap [y x])] 
      :when (not= point ".")]
      [y x point]
    )
  )
)

(defn parse [file]
  (mapv (fn [x] (clojure.string/split x #"")) (clojure.string/split (myutils/read-input file) #"\n"))
)

(defn -main [& args]
  (let [lmap (parse (first args))]
    (println "The total number of antinodes is "
      (count (apply set/union (map (fn [x] (antinodes x (count lmap) nil)) (vals (read-map lmap)))))
    )
  )

  (let [lmap (parse (first args))]
    (println "The total number of antinodes for partb is "
      (count (apply set/union (map (fn [x] (antinodes x (count lmap) true)) (vals (read-map lmap)))))
    )
  )
)

;; Call the main function
(apply -main *command-line-args*)

