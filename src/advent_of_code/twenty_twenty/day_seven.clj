(ns advent-of-code.twenty-twenty.day-seven
  (:require [clojure.string :refer [split-lines join split] :as st]
            [clojure.java.io :refer [resource]]))

(def input (split-lines (slurp (resource "day_seven_input.txt"))))

(defn parseInt [x]
  (try (Integer/parseInt x) (catch Exception _ 0)))

(defn potential-bags [colour]
  (reduce
    (fn [x y]
      (let [parsed (re-find #"(.*) bags contain (.*)\." y)]
        (if (st/includes? (last parsed) colour)
          (conj x (second parsed))
          x)))
    []
    input))

(defn bags [xs ys]
  (if (zero? (count xs))
    (set ys)
    (let [xys (mapcat concat (map potential-bags xs))]
      (bags xys (concat ys xys)))))

(defn max-bags [bag]
  (let [input (filter #(= bag (second (re-find #"(\w*\s\w*) bags?" %))) input)
        parsed (first (map #(rest (re-seq #"(\d*?)\s?(\w*\s\w*) bags?" %)) input))
        colours (mapcat (fn [[_ ttl colour]] (repeat (parseInt ttl) colour)) parsed)]
    (if (not= (count colours) 0)
      (apply concat colours (map max-bags colours))
      colours)))

(comment
  ; First part - 213
  (count (bags ["shiny gold"] []))
  
  ; Second part - 38426
  (count (max-bags "shiny gold"))
)

