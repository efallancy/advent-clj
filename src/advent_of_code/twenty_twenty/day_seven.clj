(ns advent-of-code.twenty-twenty.day-seven
  (:require [clojure.string :refer [split-lines join split] :as st]
            [clojure.java.io :refer [resource]]))

(def input (split-lines (slurp (resource "day_seven_input.txt"))))

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
      (bags xys (concat ys xys))))
)

(comment
  ; First part - 213
  (count (bags ["shiny gold"] [])))

