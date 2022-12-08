(ns day3.part1.core
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

(defn vec-in-half [coll]
  (let [len (count coll) mid-indx (/ len 2)]
    [(subvec coll 0 mid-indx) (subvec coll mid-indx len)]))

(defn -main [& _]
  (let [file-content (slurp (io/resource "rucksack.txt"))
        rsacks-str (str/split file-content #"\n")
        rsacks-chars (mapv #(vec (char-array %)) rsacks-str)
        rsacks-split (mapv vec-in-half rsacks-chars)
        rsacks-sets (mapv (fn [[e1 e2]] [(set e1) (set e2)]) rsacks-split)
        rsacks-intersect (mapv (fn [[e1 e2]] (set/intersection e1 e2)) rsacks-sets)
        rsacks-common-chars (mapv (fn [s] (get (vec s) 0)) rsacks-intersect)
        rsacks-priorities (mapv char-to-priority rsacks-common-chars)
        total (reduce + rsacks-priorities)]
    total))
