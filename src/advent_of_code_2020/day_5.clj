(ns advent-of-code-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


;; Part 1


(defn as-binary [s interp]
  (->> (seq s)
       (reduce (fn [res b]
                 (let [next (bit-shift-left res 1)
                       bit (interp b)]
                   (bit-or next bit)))
               0)))

(defn parse-seat [seat]
  (when-let [match (re-find #"^([FB]{7})([LR]{3})$" seat)]
    (let [[_ row-match column-match] match
          row (as-binary row-match {\F 0 \B 1})
          column (as-binary column-match {\L 0 \R 1})]
      {:row row
       :column column
       :id (+ (* row 8) column)})))


;; Part 1


(parse-seat "FBFBBFFRLR")
;; => {:row 44, :column 5, :id 357}

(->> '("BFFFBBFRRR"  "FFFBBBFRRR" "BBFFBBFRLL")
     (map (comp :id parse-seat))
     (into [])
     (apply max))
;; => 820

(defn parse-seats [input]
  (->> input
       (string/split-lines)
       (map parse-seat)))

(->> (slurp (io/resource "day-5"))
     parse-seats
     (map :id)
     (apply max))
;; => 996


;; Part 2

(parse-seat "BBBBBBBRRR")

(defn find-seat [seats]
  (let [sorted-seat-ids (->> seats (map :id) (into []) (sort))]
    (->> (range 0 (count sorted-seat-ids))
         (partition 2 1)
         (filter (fn [[fst snd]]
                   (= (nth sorted-seat-ids snd) (+ (nth sorted-seat-ids fst) 2))))
         (map (fn [[_ snd]] (dec (nth sorted-seat-ids snd))))
         first)))

(find-seat (parse-seats (slurp (io/resource "day-5"))))
;; => 671
