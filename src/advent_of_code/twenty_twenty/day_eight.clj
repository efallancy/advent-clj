(ns advent-of-code.twenty-twenty.day-eight
  (:require [clojure.string :refer [split-lines join split includes?] :as st]
            [clojure.java.io :refer [resource]]))

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

(defn traverse-modified [pos acc visit transformed]
  (cond
    (contains? visit pos)
    :infinite

    (= (count input) (inc pos))
    acc

    :else
    (let [[_ op v] (parse (get input pos))]
      (cond
        (= "acc" op)
        (traverse-modified (inc pos) (+ acc (parseInt v)) (conj visit pos) transformed)

        (= "jmp" op)
        (if (contains? transformed pos)
          (traverse-modified (inc pos) acc (conj visit pos) (conj transformed pos))
          (traverse-modified (+ pos (parseInt v)) acc (conj visit pos) transformed))

        :else
        (if (contains? transformed pos)
          (traverse-modified (+ pos (parseInt v)) acc (conj visit pos) (conj transformed pos))
          (traverse-modified (inc pos) acc (conj visit pos) transformed))))))

(defn traverse-fix
  []
  (let [action-map (apply merge
                          (map (fn [x y]
                                 (if (or (includes? y "jmp")
                                         (includes? y "nop"))
                                   {x y} {}))
                               (range (count input))
                               input))
        action-keys (keys action-map)]
    (first
      (filter #(not= :infinite %)
              (map #(traverse-modified 0 0 #{} #{%}) action-keys)))))

(comment
  ; First part - 1867
  (traverse 0 0 #{})

  ; Second part - ?
 (traverse-fix) 
)
