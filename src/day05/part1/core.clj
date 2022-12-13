(ns day05.part1.core
  (:gen-class)
  (:require [clojure.set :as set]))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn vec-to-groups [v group-size]
  (let [vec-w-i (map-indexed (fn [i e] {:i i :e e}) v)
        groups-of-n (group-by #(quot (get % :i) group-size) vec-w-i)
        remove-is (map-vals #(mapv (fn [m] (m :e)) %) groups-of-n)]
    remove-is))

(defn push-crate-layer [stack-map layer]
  (reduce-kv
   (fn [agg i curr-stack]
     (let [stack-to-add (if-let [new-crate (get layer i)]
                          (conj curr-stack new-crate)
                          curr-stack)]
       (assoc agg i stack-to-add)))
   {} stack-map))

(defn vec-diff [v1 v2]
  (let [v1-set (set v1)
        v2-set (set v2)]
    (vec (set/difference v1-set v2-set))))

(defn line-to-indexed-crate-map [line]
  (let [as-char-vec (vec (char-array line))
        separated-stacks (vec-to-groups as-char-vec 4)
        alpha-chars-only (map-vals #(vec-diff % [\[ \space \]]) separated-stacks)
        nonempty-vecs-only (into {} (filter #(seq (second %)) alpha-chars-only))
        vals-as-chars (map-vals first nonempty-vecs-only)]
    vals-as-chars))

(defn init-stack-map [size]
  (into {} (for [i (range size)] [i '()])))

(defn -main [& _]
  (let [line "            [J]             [B] [W]"
        layer (line-to-indexed-crate-map line)
        amt-stacks (int (Math/ceil (/ (count line) 4)))
        stack-map (init-stack-map amt-stacks)
        new-stack-map (push-crate-layer stack-map layer)]
    new-stack-map))
