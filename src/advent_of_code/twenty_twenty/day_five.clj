(ns advent-of-code.twenty-twenty.day-five
  (:require [clojure.string :refer [split-lines join]]
            [clojure.java.io :refer [resource]]))

(def input (split-lines (slurp (resource "day_five_input.txt"))))

(defn half [x] (/ x 2))

(defn seating [position-code]
  (loop [ps position-code
         rows (into [] (range 128))
         cols (into [] (range 8))]
    (if-let [x (first ps)]
      (let [pos (str x)
            ttl-rows (count rows)
            ttl-cols (count cols)]
        (cond
          (= pos "F")
          (recur (join (map str (rest ps)))
                 (subvec rows 0 (half ttl-rows))
                 cols)

          (= pos "B")
          (recur (join (map str (rest ps)))
                 (subvec rows (half ttl-rows) ttl-rows)
                 cols)

          (= pos "L")
          (recur (join (map str (rest ps)))
                 rows
                 (subvec cols 0 (half ttl-cols)))

          (= pos "R")
          (recur (join (map str (rest ps)))
                 rows
                 (subvec cols (half ttl-cols) ttl-cols))

          :else [(first rows) (first cols)]))

    [(first rows) (first cols)])))

(defn seat-id [position-code]
  (let [[row col] (seating position-code)]
    (+ (* row 8) col)))

(defn missing-seq 
  ([xs]
   (missing-seq [] xs))
  ([ms xs]
   (let [[x y] xs]
     (if (or (nil? x) (nil? y))
       (or ms [])
       (if (= 1 (- y x))
         (missing-seq (or ms []) (rest xs))
         (missing-seq (conj ms [x y]) (rest xs)))))))

(comment
  ; Sample
  (seat-id "FBFBBFFRLR")
  (seat-id "BFFFBBFRRR")
  (seat-id "FFFBBBFRRR")
  (seat-id "BBFFBBFRLL")

  ; First part - 989
  (apply max (map seat-id input))

  ; Second part - 548
  (first
    (map (fn [[x _]] (inc x))
         (missing-seq (sort (map seat-id input)))))
)

