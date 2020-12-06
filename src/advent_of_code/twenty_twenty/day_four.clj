(ns advent-of-code.twenty-twenty.day-four
  (:require [clojure.string :refer [split-lines includes?]]
            [clojure.java.io :refer [resource]]))

(def input (split (slurp (resource "day_four_input.txt")) #"\n\n"))

(defn loose-valid? [passport]
  (let [required ["byr" "iyr" "eyr" "hgt"
                  "hcl" "ecl" "pid"]
        opt ["cid"]]
    (every? #(includes? passport %) required)))

(defn valid-birth? [x]
  (<= 1920 (Integer/parseInt x) 2020))

(defn valid-issue? [x]
  (<= 2010 (Integer/parseInt x) 2020))

(defn valid-expiry? [x]
  (<= 2020 (Integer/parseInt x) 2030))

(defn valid-height? [x]
  (let [[_ value metric] (re-find #"(\d+)(cm|in)$" x)]
    (if-not (nil? metric)
      (if (= metric "cm")
        (<= 150 (Integer/parseInt value) 193)
        (<= 59 (Integer/parseInt value) 76))
      false)))

(defn valid-hair-color? [x]
  (let [[_ c] (re-find #"^#([a-f0-9]{6})$" x)]
    (not (nil? c))))

(defn valid-eye-color? [x]
  (true?
    (some (partial = x) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"])))

(defn valid-passport-id? [x]
  (let [[_ p] (re-find #"^\d{9}$" x)]
    (not (nil? p))))

(defn valid-cid? [_]
  ;ignored and will always be true
  true)

(defn ->map [x]
  (let [parsed (split x #"\s")]
    (apply merge (map #(let [[k v] (split % #":")]
                         (hash-map (keyword k) v))
                      parsed))))

(defn strict-valid? [x]
  (and (valid? x)
       (let [m (->map x)
             fn-k {:byr valid-birth?
                   :iyr valid-issue?
                   :eyr valid-expiry?
                   :hcl valid-hair-color?
                   :hgt valid-height?
                   :ecl valid-eye-color?
                   :cid valid-cid?
                   :pid valid-passport-id?}
             transformed (apply
                           merge
                           (map (fn [[k v]]
                                  {k ((get fn-k k (fn [_] false)) v)})
                                m))]
         (every? true? (vals transformed)))))

(comment
  ; First - 216 
  (count (filter true? (map loose-valid? input)))

  ; Second - 150
  (count (filter true? (map strict-valid? input)))
)

