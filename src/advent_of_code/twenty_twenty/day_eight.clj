(ns advent-of-code.twenty-twenty.day-eight
  (:require [clojure.string :refer [split-lines join split] :as st]
            [clojure.java.io :refer [resource]]))

(def sample "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(def parsed (split-lines sample))

(def input (split-lines (slurp (resource "day_eight_input.txt"))))

(defn parseInt [s]
  (try
    (Integer/parseInt s)
    (catch Exception _ 0)))

(defn parse [s]
  (re-find #"(\w+) ([+-]\d+)" s))

(defn traverse [pos acc visit]
  (if (contains? visit pos)
    acc
    (let [[_ op v] (parse (get input pos))]
      (cond
        (= "acc" op) (traverse (inc pos) (+ acc (parseInt v)) (conj visit pos))
        (= "jmp" op) (traverse (+ pos (parseInt v)) acc (conj visit pos))
        :else (traverse (inc pos) acc (conj visit pos)))
      )
    ))

(comment
  ; First part - 1867
  (traverse 0 0 #{})
)
