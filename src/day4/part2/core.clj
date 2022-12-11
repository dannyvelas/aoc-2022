(ns day4.part2.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str-range-to-map [s]
  (let [[start-str end-str] (str/split s #"-")]
    {:start (Integer/parseInt start-str)
     :end (Integer/parseInt end-str)}))

(defn range-overlaps? [r1 r2]
  (and (<= (r2 :start) (r1 :end))
       (>= (r2 :end) (r1 :start))))

(defn -main [& _]
  (let [file-content (slurp (io/resource "04-sections.txt"))
        pairs-str (str/split file-content #"\n")
        pairs-vec (mapv #(str/split % #",") pairs-str)
        pairs-map (mapv #(mapv str-range-to-map %) pairs-vec)
        pairs-full-overlap (filter #(range-overlaps? (% 0) (% 1)) pairs-map)
        amt-full-overlaps (count pairs-full-overlap)]
    amt-full-overlaps))
