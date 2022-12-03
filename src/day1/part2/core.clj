(ns day1.part2.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn add-up-vec [v]
  (reduce + v))

(defn int-vec [v]
  (map #(Integer/parseInt %) v))

(defn remove-at [v i]
  (into [] (concat (subvec v 0 i)
                   (subvec v (inc i)))))

(defn max-and-index [v]
  (let [as-map (zipmap (range (count v)) v)]
    (reverse (apply max-key val as-map))))

(defn top-n [v n]
  (if (= n 0)
    []
    (let [[max-e max-e-index] (max-and-index v)
          vec-wout-max (remove-at v max-e-index)]
      (into [] (concat [max-e] (top-n vec-wout-max (dec n)))))))

(defn -main [& _]
  (let [file-content (slurp (io/resource "calories.txt"))
        calorie-groups (str/split file-content #"\n\n")
        calorie-matrix (map #(str/split % #"\n") calorie-groups)
        calorie-matrix-num (map int-vec calorie-matrix)
        added-up-calories (map add-up-vec calorie-matrix-num)
        top-3 (top-n added-up-calories 3)]
    (println top-3)))
