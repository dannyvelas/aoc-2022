(ns day2.part1.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str-to-kw [c]
  (case c
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(def play-to-pts {:rock 1 :paper 2 :scissors 3})

(def res-to-pts {:lose 0 :draw 3 :win 6})

(def key-beats-val
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(defn get-res [they-play you-play]
  (cond
    (= they-play you-play) :draw
    (= (key-beats-val they-play) you-play) :lose
    :else :win))

(defn -main [& _]
  (let [file-content (slurp (io/resource "rock-paper-scissors.txt"))
        char-rounds (str/split file-content #"\n")
        char-rounds-matrix (mapv #(str/split % #" ") char-rounds)
        kw-rounds-matrix (mapv #(mapv str-to-kw %) char-rounds-matrix)
        play-&-res-vec (mapv (fn [round] [(second round) (apply get-res round)]) kw-rounds-matrix)
        pts-per-round (mapv (fn [[play res]] (+ (play-to-pts play) (res-to-pts res))) play-&-res-vec)
        net-points (reduce + pts-per-round)]
    net-points))
