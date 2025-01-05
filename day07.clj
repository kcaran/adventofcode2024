#!/usr/bin/env -S clojure -M

; I'm having trouble with the differenc between (map ...) and (vec map ..). It
; seems like I'm assuming that the output of the map function should be a
; vector by default? Because of course that is the way I want to use it?
;
; For the actual input, I needed to use Java's BigInteger and not just Integer
;
; PART B - I thought I was so clever using a bitfield to iterate through all
; of the combinations. But how am I supposed to iterate over 3 choices??
; Is creating a string the easiest way? Is there really any difference between
; the bitfield 0b13 (1101) and the actual string "1101"? At least in the
; 21st century where we can ignore memory size.
;
; 3^12 - 531,441. I'm having trouble figuring out how to abort out of a loop
; or return immediately out of a function.
;

(ns day07
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

; This is from co-pilot massaged a little. Mainly I couldn't figure out how
; to do it without creating a new function (which is probably better anyway)
(defn parse-line [elems]
  [
    (BigInteger. (first elems))
    (mapv #(BigInteger. %) (clojure.string/split (second elems) #"\s+"))
  ]
)

(defn parse [file]
  (mapv (fn [x] (parse-line (clojure.string/split x #":\s+"))) (clojure.string/split (myutils/read-input file) #"\n"))
)

; From copilot
(defn int-to-binary-vector [n]
  (vec (map #(Integer/parseInt (str %)) (Integer/toBinaryString n)))
)

(def tryeq [
  (fn [val x] (+ val x))
  (fn [val x] (* val x))
  (fn [val x] (BigInteger. (str val x)))
])

(defn testeq [eq mtry]
  (if (= (eq 0)
    (reduce
      (fn [val i]
        (if (zero? (bit-and mtry (bit-shift-left 1 i)))
          (+ val ((eq 1) (inc i)))
          (* val ((eq 1) (inc i)))
        )
      ) ((eq 1) 0)
      (range (dec (count (eq 1))))
    ))
    1
    0
  )
)

(defn equate [eq]
  (loop [i 0]
    (if (>= i (int (Math/pow 2 (dec (count (eq 1))))))
    0
    (if (= (testeq eq i) 1)
      (eq 0)
      (recur (inc i))
    )
    )
  )
)

(defn equateb [eq]
  (loop [queue [[(nth (eq 1) 0) 0]]]
    (if (nil? (first queue))
    0
    (let [mtry (first queue)]
      (if (= (nth mtry 1) (dec (count (eq 1))))
        (if (= (nth mtry 0) (nth eq 0))
           (nth eq 0)
           (recur (next queue))
        )
        (if (> (nth mtry 0) (nth eq 0))
           (recur (next queue))
           (recur (conj (next queue)
             [ (+ (nth mtry 0) (nth (eq 1) (inc(nth mtry 1)))) (inc(nth mtry 1)) ]
             [ (* (nth mtry 0) (nth (eq 1) (inc(nth mtry 1)))) (inc(nth mtry 1)) ]
             [ (BigInteger. (str (nth mtry 0) (nth (eq 1) (inc(nth mtry 1))))) (inc(nth mtry 1)) ]
             ))
        )
      )
    )
    )
  )
)

(defn -main [& args]
  (println "The total calibration result is "
    (reduce +
      (map (fn [e] (equateb e)) (parse (first args)))
    )
  )
)

;; Call the main function
(apply -main *command-line-args*)

