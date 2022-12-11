(ns day04.part1.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str-range-to-map [s]
  (let [[start-str end-str] (str/split s #"-")]
    {:start (Integer/parseInt start-str)
     :end (Integer/parseInt end-str)}))

(defn range-fully-contains? [r1 r2]
  (<= (r1 :start) (r2 :start) (r2 :end) (r1 :end)))

(defn -main [& _]
  (let [file-content (slurp (io/resource "04-sections.txt"))
        pairs-str (str/split file-content #"\n")
        pairs-vec (mapv #(str/split % #",") pairs-str)
        pairs-map (mapv #(mapv str-range-to-map %) pairs-vec)
        pairs-full-overlap (filter #(or (range-fully-contains? (% 0) (% 1))
                                        (range-fully-contains? (% 1) (% 0))) pairs-map)
        amt-full-overlaps (count pairs-full-overlap)]
    amt-full-overlaps))
