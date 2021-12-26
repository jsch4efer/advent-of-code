(ns advent-of-code.2021.day-16
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))




;; bit 0-2 = package version (numbers)
;; bit 3-5 = type id (numbers)
;;  type id = 4 => literal values (single binary number padded to be multiple of 4)
;;  broken into groups of 5 bit containing 4 bits (all prefixed by 1 bit except last with 0)


(def code {\0 "0000"
           \1 "0001"
           \2 "0010"
           \3 "0011"
           \4 "0100"
           \5 "0101"
           \6 "0110"
           \7 "0111"
           \8 "1000"
           \9 "1001"
           \A "1010"
           \B "1011"
           \C "1100"
           \D "1101"
           \E "1110"
           \F "1111"})

(defn hex->bin [hex]
  (->> (seq hex)
       (mapcat code)
       (reduce str)))

(defn bin->dec [bin]
  (Long/parseLong bin 2))

(def input (first (string/split-lines (slurp (io/resource "2021/day-16")))))

(defn parse-literal [data start]
  (loop [index start
         digits ""]
    (let [next-index (+ index 5)]
      (case (nth data index)
        \0 [start (dec next-index) (bin->dec (str digits (subs data (inc index) next-index)))]
        \1 (recur next-index (str digits (subs data (inc index) next-index)))))))

(defn parse-bits-package
  ([data index]
   (let [version (bin->dec (subs data index (+ index 3)))
         type (bin->dec (subs data (+ index 3) (+ index 6)))
         body-start (+ index 6)]
     (println "Version" version, "Type" type, "Body start" body-start)
     (case type
       4
       (let [[start end val] (parse-literal data body-start)]
         (println "Parsed literal:" start end val)
         {:type type
          :version version
          :start index
          :end end
          :value val})
       (let [start body-start
             operator-mode (nth data start)
             start-index (+ start (case operator-mode \0 16 \1 12))
             length (case operator-mode
                      \0 (bin->dec (subs data (inc start) (+ start 16)))
                      \1 (bin->dec (subs data (inc start) (+ start 12))))]
         (println "Length" length)
         (let [[end subs] (loop [index start-index
                                 remaining length
                                 subs []]
                            (if (and (< index (count data))
                                     (case operator-mode \0 (<= 6 remaining) \1 (< 0 remaining)))
                              (let [{:keys [start end] :as sub} (parse-bits-package data index)
                                    next-remaining (- remaining
                                                      (case operator-mode \0 (inc (- end start)) \1 1))]
                                (recur (inc end) next-remaining (conj subs sub)))
                              [(+ (:end (last subs)) (case operator-mode \0 remaining 0)) subs]))]

           (println "Parsed operator:" index end)
           {:type type
            :version version
            :start index
            :end end
            :subs subs})))))

  ([data]
   (println "Parsing bit package:" data)
   (println "Max index" (dec (count data)))
   (let [result
         (parse-bits-package data 0)]
     (println result)
     result)))

(defn get-version [expr]
  (case (:type expr)
    4 [(:version expr)]
    (->> expr :subs (mapcat get-version) (into [(:version expr)]))))

(defn eval-expr [expr]
  (let [op (case (:type expr)
             0 +
             1 *
             2 min
             3 max
             4 :value
             5 (comp #(if % 1 0) >)
             6 (comp #(if % 1 0) <)
             7 (comp #(if % 1 0) =))]
    (case (:type expr)
      4 (:value expr)
      (apply op (->> (:subs expr) (map eval-expr))))))

(comment
    ;; Part 1

  (parse-bits-package (hex->bin "D2FE28"))

  (parse-bits-package (hex->bin "EE00D40C823060"))

  (->> (parse-bits-package (hex->bin "8A004A801A8002F478"))
       (get-version)
       (reduce +))
    ;; => 16
    ;;
  (->> (parse-bits-package (hex->bin "620080001611562c8802118e34"))
       (get-version)
       (reduce +))
;; => 12

  (->> (parse-bits-package (hex->bin "C0015000016115A2E0802F182340"))
       (get-version)
       (reduce +))
;; => 23
  (->> (parse-bits-package (hex->bin input))
       (get-version)
       (reduce +))

  (eval-expr (parse-bits-package (hex->bin "C200B40A82")))
;; => 3
  (eval-expr (parse-bits-package (hex->bin "9C0141080250320F1802104A08")))
;; => 1

  (eval-expr (parse-bits-package (hex->bin input)))
;; => 739303923668
  )
