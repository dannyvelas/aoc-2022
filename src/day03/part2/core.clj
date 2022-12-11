(ns day03.part2.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def ascii {:A 65 :Z 90 :a 97})

(def base-priority {:A 27 :a 1})

(defn is-uppercase [ascii-code] (<= (ascii :A) ascii-code (ascii :Z)))

(defn char-to-priority [c]
  (let [as-int (int c)
        root-letter (cond (is-uppercase as-int) :A :else :a)
        index-of-char (- as-int (ascii root-letter))]
    (+ index-of-char (base-priority root-letter))))

(defn get-common-char [vecs]
  (let [as-sets (map set vecs)
        intersection (apply set/intersection as-sets)
        intersection-as-char (get (vec intersection) 0)]
    intersection-as-char))

(defn nest-vec [v group-size]
  (let [vec-w-i (map-indexed (fn [i e] {:i i :e e}) v)
        groups-of-n (group-by #(quot (get % :i) group-size) vec-w-i)
        as-vec (mapv (fn [[_ vec-of-maps]] (mapv #(get % :e) vec-of-maps)) groups-of-n)]
    as-vec))

(defn -main [& _]
  (let [file-content (slurp (io/resource "03-rucksack.txt"))
        rsacks-str (str/split file-content #"\n")
        rsacks-chars (mapv #(vec (char-array %)) rsacks-str)
        rsacks-grps-of-3 (nest-vec rsacks-chars 3)
        rsacks-common-items (mapv get-common-char rsacks-grps-of-3)
        priorities (mapv char-to-priority rsacks-common-items)
        summed-priorities (reduce + priorities)]
    summed-priorities))
