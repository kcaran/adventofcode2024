#!/usr/bin/env -S clojure -M

(ns day12
(:require [clojure.string :refer [split trim]])
(:require [myutils :refer [read-input]])
)

(defn f-up [lmap pos]
  (if (or (zero? (pos 0))
           (not= (get-in lmap [(dec (pos 0)) (pos 1)]) (get-in lmap pos)))
    1 0
  )
)

(defn f-down [lmap pos]
  (if (or (= (pos 0) (dec (count lmap)))
           (not= (get-in lmap [(inc (pos 0)) (pos 1)]) (get-in lmap pos)))
    1 0
  )
)

(defn f-left [lmap pos]
  (if (or (zero? (pos 1))
           (not= (get-in lmap [(pos 0) (dec (pos 1))]) (get-in lmap pos)))
    1 0
  )
)

(defn f-right [lmap pos]
  (if (or (= (pos 1) (dec (count lmap)))
           (not= (get-in lmap [(pos 0) (inc (pos 1))]) (get-in lmap pos)))
    1 0
  )
)

(defn up [lmap pos]
  (if (zero? (pos 0))
    nil
    (if (= (get-in lmap [(dec (pos 0)) (pos 1)]) (get-in lmap pos))
       [(dec (pos 0)) (pos 1)]
       nil
    )
  )
)

(defn down [lmap pos]
  (if (= (pos 0) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(inc (pos 0)) (pos 1)]) (get-in lmap pos))
       [(inc (pos 0)) (pos 1)]
       nil
    )
  )
)

(defn right [lmap pos]
  (if (= (pos 1) (dec (count lmap)))
    nil
    (if (= (get-in lmap [(pos 0) (inc (pos 1))]) (get-in lmap pos))
       [(pos 0) (inc (pos 1))]
       nil
    )
  )
)

(defn left [lmap pos]
  (if (zero? (pos 1))
    nil
    (if (= (get-in lmap [(pos 0) (dec (pos 1))]) (get-in lmap pos))
       [(pos 0) (dec (pos 1))]
       nil
    )
  )
)

(def has-fence [
   f-up
   f-down
   f-right
   f-left
  ]
)

(def move [
   up
   down
   right
   left
  ]
)

(defn height [lmap pos]
  (get-in lmap (vec (map (fn [x] (Integer. x)) (clojure.string/split pos #","))))
)

(defn visit [lmap start]
  (loop [moves [ start ] visited #{} ]
    ; If there are no moves left
    (if (nil? moves)
      visited
      (let [pos (first moves)]
        (if (nil? (visited pos))
          (recur
            (into (next moves) (remove nil? (map (fn [m] ((move m) lmap pos)) (range 4))))
            (conj visited pos)
          )
          (recur (next moves) visited)
        )
      )
    )
  )
)

(defn traverse [lmap]
  (loop [y 0 x 0 visited #{} regions []]
    (if (>= y (count lmap))
      regions
      (if (>= x (count (first lmap)))
         (recur (inc y) 0 visited regions)
         (if (visited [y x])
           (recur y (inc x) visited regions)
           (let [region (visit lmap [y x])]
             (recur y (inc x) (into visited region) (conj regions region))
           )
         )
      )
    )
  )
)

(defn fence [lmap region]
  (reduce +
    (map (fn [pos] (reduce + (map (fn [i] ((has-fence i) lmap pos)) (range 4)))) region)
  )
)

(defn calc-price [lmap]
  (let [regions (traverse lmap)]
  (reduce +
    (map (fn [r] (* (fence lmap r) (count r))) regions)
  )
  )
)

(defn parse [str]
  (vec
    (map (fn [y] (vec (clojure.string/split y #""))) (clojure.string/split str #"\n"))
  )
)

(defn -main [& args]
  (let [lmap (parse (myutils/read-input (first args)))]
    (println "The price of fences is " (calc-price lmap))
  )
)

;; Call the main function
(when-not (empty? *command-line-args*)
  (apply -main *command-line-args*)
)

