(ns advent-of-code.twenty-twenty.day-six
  (:require [clojure.string :refer [split-lines join split]]
            [clojure.java.io :refer [resource]]))

(def input (split (slurp (resource "day_six_input.txt")) #"\n\n"))

(defn sum-any-answered [xs]
  (apply + (map #(-> % (split #"\s") join set count) xs)))

(defn exact-answer [x]
  (let [answers (split x #"\s")
        participants (count answers)
        joined-answers (join answers)]
    (apply merge
           (map (fn [[k v]]
                  (if (= participants v) {k v} {}))
                (frequencies joined-answers)))))

(defn sum-all-answered [xs]
  (let [parsed (map exact-answer xs)]
    (apply + (map #(-> % keys count) parsed))))

(comment
  ; First part - 6903
  (sum-any-answered input)

  ; Second part - ?
  (sum-all-answered input)
)

