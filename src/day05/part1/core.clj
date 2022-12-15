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

(defn line-to-layer [line]
  (let [as-char-vec (vec (char-array line))
        separated-stacks (vec-to-groups as-char-vec crate-str-len)
        alpha-chars-only (map-vals #(vec-diff % [\[ \space \]]) separated-stacks)
        nonempty-vecs-only (into {} (filter #(seq (second %)) alpha-chars-only))
        vals-as-chars (map-vals first nonempty-vecs-only)]
    vals-as-chars))

(defn stacks-section-to-layer-vec [stacks-section]
  (let  [split-by-nl (str/split stacks-section #"\n")
         rm-col-names (subvec split-by-nl 0 (dec (count split-by-nl)))
         vec-of-maps (mapv line-to-layer rm-col-names)]
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

(defn init-map-of-stacks [stacks-section]
  (let [amt-stacks (calc-amt-stacks stacks-section)
        empty-stack-map (into {} (for [i (range amt-stacks)] [i '()]))
        layers (stacks-section-to-layer-vec stacks-section)
        filled-stack-map (push-all-layers empty-stack-map layers)
        stack-map-correct-order (map-vals reverse filled-stack-map)]
    stack-map-correct-order))

(defn init-cmd-vec [cmd-section]
  (let [split-by-nl (str/split cmd-section #"\n")
        split-by-sp (mapv #(str/split % #" ") split-by-nl)
        only-nums (mapv (fn [v] {:amt (v 1) :from (v 3) :to (v 5)}) split-by-sp)
        parsed-nums (mapv (fn [m] (map-vals #(Integer/parseInt %) m)) only-nums)]
    parsed-nums))

(defn move [stack-map from-index to-index]
  (let [from-stack (stack-map from-index)
        to-stack (stack-map to-index)
        crate-being-moved (peek from-stack)
        new-from-stack (pop from-stack)
        new-to-stack (conj to-stack crate-being-moved)
        stack-map-new-from (assoc stack-map from-index new-from-stack)
        stack-map-new-to (assoc stack-map-new-from to-index new-to-stack)]
    stack-map-new-to))

(defn exec-cmd [stack-map cmd]
  (loop [i 0 stack-map stack-map]
    (if (< i (cmd :amt))
      (let [new-stack-map (move stack-map (cmd :from) (cmd :to))]
        (recur (inc i) new-stack-map))
      stack-map)))

(defn exec-cmds [stack-map cmds]
  (loop [i 0 stack-map stack-map]
    (if (< i (count cmds))
      (let [new-map (exec-cmd stack-map (cmds i))]
        (recur (inc i) new-map))
      stack-map)))

(defn -main [& _]
  (let [file-content (slurp (io/resource "05-crates.txt"))
        [stacks-section cmd-section] (str/split file-content #"\n\n")
        stack-map (init-map-of-stacks stacks-section)
        cmds (init-cmd-vec cmd-section)
        stack-map-result (exec-cmds stack-map cmds)]
    stack-map-result))
