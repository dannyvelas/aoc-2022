(ns day1.part1.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn add-up-vec [v]
  (reduce + v))

(defn int-vec [v]
  (map #(Integer/parseInt %) v))

(defn -main [& _]
  (let [file-content (slurp (io/resource "01-calories.txt"))
        calorie-groups (str/split file-content #"\n\n")
        calorie-matrix (map #(str/split % #"\n") calorie-groups)
        calorie-matrix-num (map int-vec calorie-matrix)
        added-up-calories (map add-up-vec calorie-matrix-num)
        max-calories (apply max added-up-calories)]
    (println max-calories)))
