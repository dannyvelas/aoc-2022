(defproject aoc-2022 "0.1.0-SNAPSHOT"
  :description "AOC 2022 Solutions"
  :url "https://github.com/dannyvelas/aoc-2022"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main nil
  :target-path "target/%s"
  :profiles {:day1-part1 {:main day1.part1.core}
             :day1-part2 {:main day1.part2.core}})
