(ns day1.part2.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn add-up-vec [v]
  (reduce + v))

(defn int-vec [v]
  (map #(Integer/parseInt %) v))

(defn top-n [tally-map n]
  (if (= n 0)
    []
    (let [[-max-key freq] (apply max-key key tally-map)
          new-map (if (= freq 1)
                    (dissoc tally-map -max-key)
                    (assoc tally-map -max-key (dec freq)))]
      (into [] (concat [-max-key] (top-n new-map (dec n)))))))

(defn tally
  ([v] (tally v {} 0))
  ([v as-map i]
   (if (< i (count v))
     (let [e (get v i)
           v-in-map (get as-map e)
           new-v (if (not= v-in-map nil) (inc v-in-map) 1)]
       (tally v (assoc as-map e new-v) (inc i)))
     as-map)))

(defn -main [& _]
  (let [file-content (slurp (io/resource "calories.txt"))
        calorie-groups (str/split file-content #"\n\n")
        calorie-matrix (mapv #(str/split % #"\n") calorie-groups)
        calorie-matrix-num (mapv int-vec calorie-matrix)
        calorie-sums (mapv add-up-vec calorie-matrix-num)
        calorie-sums-map (tally calorie-sums)
        top-3 (top-n calorie-sums-map 3)
        top-3-sum (add-up-vec top-3)]
    top-3-sum))
