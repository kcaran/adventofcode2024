#!/usr/bin/env -S clojure -M

; part b - How do you define a loop? For part b the direction has to the same
; as well as the position.

(ns day15
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(declare move)

(defn prmap [lmap]
  (let [mp (assoc-in (vec (repeat (lmap :max) (vec (repeat (lmap :max) '.)))) (lmap :bot) '\@)]
    (run! (fn [l] (println l))
      (reduce (fn [mpp pt] (assoc-in mpp pt '\#))
      (reduce (fn [mpp pt] (assoc-in mpp pt 'O))
        mp
        (lmap :rocks)
      )
        (lmap :walls)
      )
    )
  )
)

(defn newpos [lmap dir pos]
  (case dir
    "^" [(dec (pos 0)) (pos 1)]
    "v" [(inc (pos 0)) (pos 1)]
    ">" [(pos 0) (inc (pos 1))]
    "<" [(pos 0) (dec (pos 1))]
  )
)

(defn move-bot [lmap dir]
(println (lmap :bot) dir)
(prmap lmap)
  (move lmap dir (lmap :bot))
)

(defn move-rock [lmap dir pos newp]
  (update lmap :rocks (fn [r] (conj (disj r pos) newp)))
)

(defn push-rock [lmap dir pos]
  (loop [newp (newpos lmap dir pos)]
(println "KAC" dir pos newp)
    (if ((lmap :walls) newp)
      lmap
      (if ((lmap :rocks) newp)
        (recur (newpos lmap dir newp))
        (move-rock lmap dir pos newp)
      )
    )
  )
)

(defn move [lmap dir pos]
  (let [lm lmap newp (newpos lmap dir pos)]
    (if ((lmap :walls) newp)
      lmap
      (if ((lmap :rocks) newp)
        (let [lm (push-rock lmap dir newp)]
          (if ((lm :rocks) newp)
            lmap
            (assoc lm :bot newp)
          )
        )
        (assoc lmap :bot newp)
      )
    )
  )
)

; Having issues with start returning a list of vectors and not a single vector
(defn traverse [lmap dirs]
  (reduce move-bot lmap dirs)
)

; From llm
(defn char-locations [grid]
  (reduce (fn [locations [y row]]
     (reduce (fn [locations [x char]]
          (assoc locations char (conj (get locations char []) [y x])))
        locations
        (map-indexed vector row)
     ))
    {}
    (map-indexed vector grid)
  )
)

(defn parse-map [mstr]
  (let [m (mapv (fn [x] (clojure.string/split x #"")) (clojure.string/split mstr #"\n")) loc (char-locations m)]
    { :bot (first (loc "@")) :walls (set (loc "#")) :rocks (set (loc "O")) :max (count (clojure.string/split mstr #"\n")) }
  )
)

(defn parse-dirs [dstr]
  (vec (filter #(not (empty? %)) (clojure.string/split dstr #"\n?")))
)

(defn parse [file]
  (let [[lmap dirs] (clojure.string/split (myutils/read-input file) #"\n\n")]
    [ (parse-map lmap) (parse-dirs dirs) ]
  )
)

(defn score [lmap]
  (reduce +
    (map (fn [c] (+ (* 100 (c 0)) (c 1))) (lmap :rocks))
  )
)

(defn -main [& args]
  (let [[lmap dirs ] (parse (first args))]
    (println lmap dirs)
    (println "The sum of the GPS coords is " (score (traverse lmap dirs)))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

