(ns day05.part2.core
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

(defn push-one-layer [stack-vec layer]
  (vec (for [[i stack] (mapv vector (range 0 (count stack-vec)) stack-vec)]
         (if-let [new-crate (get layer i)]
           (conj stack new-crate)
           stack))))

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

(defn push-all-layers [empty-stack-vec layers]
  (loop [i 0 stack-vec empty-stack-vec]
    (cond (< i (count layers))
          (let [new-stack-vec (push-one-layer stack-vec (get layers i))]
            (recur (inc i) new-stack-vec))
          :else stack-vec)))

(defn init-vec-of-stacks [stacks-section]
  (let [amt-stacks (calc-amt-stacks stacks-section)
        empty-stack-vec (vec (repeat amt-stacks '()))
        layers (stacks-section-to-layer-vec stacks-section)
        filled-stack-vec (push-all-layers empty-stack-vec layers)
        stack-vec-correct-order (mapv reverse filled-stack-vec)]
    stack-vec-correct-order))

(defn init-cmd-vec [cmd-section]
  (let [split-by-nl (str/split cmd-section #"\n")
        split-by-sp (mapv #(str/split % #" ") split-by-nl)
        vec-of-maps (mapv (fn [v] {:amt (v 1) :from (v 3) :to (v 5)}) split-by-sp)
        vec-of-int-maps (mapv (fn [m] (map-vals #(Integer/parseInt %) m)) vec-of-maps)
        zero-indexed (mapv #(let [dec-from (assoc % :from (dec (% :from)))
                                  dec-to (assoc dec-from :to (dec (% :to)))]
                              dec-to) vec-of-int-maps)]
    zero-indexed))

(defn move [stack-vec cmd]
  (let [from-stack (stack-vec (cmd :from))
        to-stack (stack-vec (cmd :to))
        crates-being-moved (take (cmd :amt) from-stack)
        new-from-stack (drop (cmd :amt) from-stack)
        new-to-stack (concat crates-being-moved to-stack)
        stack-vec-new-from (assoc stack-vec (cmd :from) new-from-stack)
        stack-vec-new-to (assoc stack-vec-new-from (cmd :to) new-to-stack)]
    stack-vec-new-to))

(defn exec-cmds [stack-vec cmds]
  (loop [i 0 stack-vec stack-vec]
    (if (< i (count cmds))
      (let [new-vec (move stack-vec (cmds i))]
        (recur (inc i) new-vec))
      stack-vec)))

(defn -main [& _]
  (let [file-content (slurp (io/resource "05-crates.txt"))
        [stacks-section cmd-section] (str/split file-content #"\n\n")
        stack-vec (init-vec-of-stacks stacks-section)
        cmds (init-cmd-vec cmd-section)
        stack-vec-result (exec-cmds stack-vec cmds)
        top-crates (map first stack-vec-result)
        concatted (apply str top-crates)]
    concatted))
