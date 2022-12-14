(ns day05.part1.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def crate-str-len 4)

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn vec-to-groups [-vec group-size]
  (reduce-kv (fn [agg k v]
               (let [group-i (quot k group-size)]
                 (if-let [group-so-far (get agg group-i)]
                   (assoc agg group-i (conj group-so-far v))
                   (assoc agg group-i [v]))))
             {}
             -vec))

(defn push-one-layer [stack-map layer]
  (into {} (for [[i stack] stack-map]
             (if-let [new-crate (get layer i)]
               [i (conj stack new-crate)]
               [i  stack]))))

(defn vec-diff [v1 v2]
  (let [v1-set (set v1)
        v2-set (set v2)]
    (vec (set/difference v1-set v2-set))))

(defn line-to-indexed-crate-map [line]
  (let [as-char-vec (vec (char-array line))
        separated-stacks (vec-to-groups as-char-vec crate-str-len)
        alpha-chars-only (map-vals #(vec-diff % [\[ \space \]]) separated-stacks)
        nonempty-vecs-only (into {} (filter #(seq (second %)) alpha-chars-only))
        vals-as-chars (map-vals first nonempty-vecs-only)]
    vals-as-chars))

(defn stacks-section-to-vec-of-maps [stacks-section]
  (let  [split-by-nl (str/split stacks-section #"\n")
         no-col-names (subvec split-by-nl 0 (dec (count split-by-nl)))
         vec-of-maps (mapv line-to-indexed-crate-map no-col-names)]
    vec-of-maps))

(defn calc-amt-stacks [stacks-section]
  (let [single-line (get (str/split stacks-section #"\n") 0)]
    (int (Math/ceil (/ (count single-line) crate-str-len)))))

(defn push-all-layers [empty-stack-map layers]
  (loop [i 0 stack-map empty-stack-map]
    (cond (< i (count layers))
          (let [new-stack-map (push-one-layer stack-map (get layers i))]
            (recur (inc i) new-stack-map))
          :else stack-map)))

(defn stacks-section-to-map [stacks-section]
  (let [amt-stacks (calc-amt-stacks stacks-section)
        empty-stack-map (into {} (for [i (range amt-stacks)] [i '()]))
        layers (stacks-section-to-vec-of-maps stacks-section)
        filled-stack-map (push-all-layers empty-stack-map layers)
        stack-map-correct-order (map-vals reverse filled-stack-map)]
    stack-map-correct-order))

(defn -main [& _]
  (let [file-content (slurp (io/resource "05-crates.txt"))
        [stacks-section instructions-section] (str/split file-content #"\n\n")
        stack-map (stacks-section-to-map stacks-section)]
    stack-map))
