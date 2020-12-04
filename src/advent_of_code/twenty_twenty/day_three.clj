(ns advent-of-code.twenty-twenty.day-one
  (:require [clojure.string :refer [split-lines]]
            [clojure.java.io :refer [resource]]))

(def input (split-lines (slurp (resource "day_three_input.txt"))))

(defn transcode [line idx]
  (if (not (> (count line) idx))
    (transcode (str line line) idx)
    (let [c (get line idx)]
      (if (= "#" (str c)) "X" "O"))))

(defn traverse-tree
  [input right down]
  (let [lines (rest input) ; First line ignored
        idxs  (let [x (atom 0)]
                (map #(if (= 0 (mod (inc %) down))
                        (swap! x (fn [z] (+ right z)) ) -1)
                     (range (count lines))))]
    (count
      (filter (partial = "X")
              (map #(transcode %1 %2) lines idxs)))))

(defn first-part
  [input]
  (traverse-tree input 3 1))


(defn second-part [input xs]
  (apply * (map #(apply (partial traverse-tree input) %) xs)))

(comment
  ; 153
  (first-part input)

  ; 2421944712
  (second-part input [[1 1] [3 1] [5 1] [7 1] [1 2]]))

