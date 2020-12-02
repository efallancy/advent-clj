(ns advent-of-code.twenty-twenty.day-two
  (:require [clojure.string :refer [split-lines split]]
            [clojure.java.io :refer [resource]]
            [clojure.walk :refer [keywordize-keys]]))

; Ref: https://adventofcode.com/2020/day/2

(def input (split-lines (slurp (resource "day_two_input.txt"))))

(defn first-part
  []
  (count
    (for [i input
        :let [parsed (split i #" ")
              [a b] (map #(Integer/parseInt %) (split (first parsed) #"-"))
              l (first (split (second parsed) #":"))
              words (split (last parsed) #"")]
        :when (<= a (get (frequencies words) l 0) b)]
    [1])))


(defn second-part
  []
  (count
    (for [i input 
        :let [parsed (split i #" ")
              [a b] (map #(Integer/parseInt %) (split (first parsed) #"-"))
              l (first (split (second parsed) #":"))
              words (split (last parsed) #"")]
        :when (if (not (and (= (str (get words (dec a) "")) l)
                            (= (str (get words (dec b) "")) l)))
                (or (= (str (get words (dec a) "")) l)
                    (= (str (get words (dec b) "")) l))
                false)]
    [1])))


(comment
  ; 582
  (first-part)
  ; 729
  (second-part))
)
