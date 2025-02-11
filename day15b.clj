#!/usr/bin/env -S clojure -M

; part b - How do you define a loop? For part b the direction has to the same
; as well as the position.

(ns day15
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(declare move)

(defn prmap [lmap]
  (println (lmap :rocks))
  (let [mp (assoc-in (vec (repeat (lmap :max) (vec (repeat (* (lmap :max) 2) '.)))) (lmap :bot) '\@)]
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
(prmap lmap)
  (move lmap dir (lmap :bot))
)

;(defn move-rock [lmap dir pos newp]
;  (println "Moving rock from " pos "to" newp)
;  (update lmap :rocks (fn [r] (into (disj r pos)
;    (case dir
;      "<" (map (fn [x] [(pos 0) x]) (range (newp 1) (pos 1) 2))
;      ">" (map (fn [x] [(pos 0) x]) (range (pos 1) (newp 1) 2))
;      "^" (map (fn [y] [y (pos 1)]) (range (pos 0) (newp 0) 2))
;      "v" [newp]
;    )
;   )))
;)

(defn move-rock [lmap dir pos newp]
  (println "Moving rock from " pos "to" newp)
  (update lmap :rocks (fn [r] (into (disj r pos)
    (case dir
      "<" (map (fn [x] [(pos 0) x]) (range (newp 1) (pos 1) 2))
      ">" (map (fn [x] [(pos 0) x]) (range (pos 1) (newp 1) 2))
      "^" (map (fn [y] [y (pos 1)]) (range (pos 0) (newp 0) 2))
      "v" [newp]
    )
   )))

(defn test-rock [lmap dir pos]
 (println "testing: " pos ((lmap :rocks) [ (pos 0) (- (pos 1) 1) ]) )
    (or ((lmap :rocks) [ (pos 0) (- (pos 1) 1) ])
        ((lmap :rocks) pos))
)

(defn push-rock [lmap dir pos]
  (loop [newp (newpos lmap dir pos)]
;(println "KAC" dir pos newp)
    (if ((lmap :walls) newp)
      lmap
      (if (test-rock lmap dir newp)
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
   (let [rock (test-rock lmap dir newp)]
	(if rock
	 (let [lm (push-rock lmap dir rock)]
	  (println "lm is now" (lm :rocks) rock)
	  (if ((lm :rocks) rock)
       (do (println "ROCK!")
	   lmap)
       (do (println "NOW" (assoc lm :bot rock))
	   (assoc lm :bot rock)
)
	  )
	 )
	 (assoc lmap :bot newp)
	)
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
     (reduce (fn [locations [x chr]]
          (case chr
            "#" (assoc locations chr
                  (into (get locations chr []) [ [y (* x 2)] [y (+ (* x 2) 1)] ])
                )
            (assoc locations chr
                  (into (get locations chr []) [ [y (* x 2)] ])
            )
          )
        )
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
  (let [[lmap dirs] (parse (first args))]
    (println lmap dirs)
    (println "The sum of the GPS coords is " (score (traverse lmap dirs)))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

