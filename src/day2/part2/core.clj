(ns day2.part2.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def str-to-result {"X" :lose "Y" :draw "Z" :win})

(def str-to-play {"A" :rock "B" :paper "C" :scissors})

(def play-to-pts {:rock 1 :paper 2 :scissors 3})

(def res-to-pts {:lose 0 :draw 3 :win 6})

(def key-beats-val
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(defn get-your-play [{their-play :their-play result :result}]
  (case result
    :draw their-play
    :lose (key-beats-val their-play)
    :win ((set/map-invert key-beats-val) their-play)))

(defn parse-str-round [[their-play result]]
  {:their-play (str-to-play their-play)
   :result (str-to-result result)})

(defn calc-pts [{your-play :your-play result :result}]
  (+ (play-to-pts your-play) (res-to-pts result)))

(defn -main [& _]
  (let [file-content (slurp (io/resource "02-rock-paper-scissors.txt"))
        str-rounds (str/split file-content #"\n")
        str-rounds (mapv #(str/split % #" ") str-rounds)
        rounds (mapv parse-str-round str-rounds)
        rounds-w-your-play (mapv #(assoc % :your-play (get-your-play %)) rounds)
        pts-per-round (mapv calc-pts rounds-w-your-play)
        net-points (reduce + pts-per-round)]
    net-points))
