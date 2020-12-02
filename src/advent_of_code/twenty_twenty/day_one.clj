(ns advent-of-code.twenty-twenty.day-one
  (:require [clojure.string :refer [split]]))

; Ref: https://adventofcode.com/2020/day/1

(def input
  (let [file (slurp "resources/day_one_input.txt")
        parsed (split file #"\n")]
    (map (fn [x] (Integer/parseInt x)) parsed)))

(defn find-match-pair
  "Find the matching pair that sums up to the amount"
  ([xs] (find-match-pair xs 2020))
  ([xs amount]
   (if-let [ttl (count xs)]
     (loop [i 0]
       (when (< i ttl)
         (let [x (nth xs i)
               pair (- amount x)
               set-list (set xs)]
           (if (contains? set-list pair)
             [x pair]
             (recur (inc i)))))))))

(defn first-part
  "Find matching pair and multiply all"
  [input]
  (->> input find-match-pair (apply *)))

(defn find-match-triplet
  "Find matching triplet that sums up to the amount"
  ([xs] (find-match-triplet xs 2020))
  ([xs amount]
   (if-let [ttl (count xs)] 
     (loop [i 0]
       (when (< i ttl)
         (let [x (nth xs i)
               pair (find-match-pair xs (- amount x))]
           (if pair 
             (conj pair x)
             (recur (inc i)))))))))

(defn second-part
  "Find matching triplet and multiply all"
  [xs]
  (->> input find-match-triplet (apply *)))

(comment
  ; Answer - 482811
  (first-part input)
  ; Answer - 193171814
  (second-part input)

  ; Alternative (simple and slick) implementation based of Lambda Island
  ; First part
  (set
    (for [x input
          y input
          :when (= 2020 (+ x y))]
      (* x y)))

  ; Second part
  (set
    (for [x input
          y input
          z input
          :when (= 2020 (+ x y z))]
      (* x y z)))
)
