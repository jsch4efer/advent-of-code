(ns advent-of-code.2021.day-14
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn parse-polymerization [input]
  (let [[top bottom] (->> (string/split-lines input)
                          (split-with not-empty))
        rules (->> bottom
                   (drop 1)
                   (map (fn [rule] (string/split rule #" -> ")))
                   (into {}))

        template (first top)]
    [template rules]))

(defn polymer-process [[template rules] steps rating]
  (->> (iterate (fn [[state step]]
                  (println "Step" step "State" state)
                  (loop [s '()
                         t state]
                    (let [t1 (first t)
                          t2 (second t)]
                      (if (and t1 t2)
                        (let [r (rules (str t1 t2))
                              sn (cons (str t1) s)
                              tn (rest t)]
                          (recur (if r (cons r sn) sn) tn))
                        (if t1
                          (recur (cons (str t1) s) (rest t))
                          [(reverse s) (inc step)])))))
                [template 0])
       (drop steps)
       first
       first
       rating))

(defn polymer-process-faster [[template rules] steps rating]
  (let [init (->> template
                  (partition 2 1)
                  (map (partial string/join ""))
                  (reduce (fn [res next]
                            (update res next #(if % (inc %) 1)))
                          {}))]
    (->> (iterate (fn [[state step]]
                    (println "Step" step, "State" state)
                    [(->> rules
                          (mapcat (fn [[seg rep]]
                                    (if-let [seg-count (get state seg 0)]
                                      [[seg (* -1 seg-count)]
                                       [(str (first seg) rep) seg-count]
                                       [(str rep (second seg)) seg-count]]
                                      [])))
                          (reduce (fn [res [seg count]]
                                    (update res seg
                                            (fn [old]
                                              (if old
                                                (+ old count)
                                                count))))
                                  state))
                     (inc step)])
                  [init 0])
         (drop steps)
         first
         first
         rating)))

(comment

  (def example "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
")

;; Part 1

  (def rating-1 (fn [state]
                  (let [freqs (->> state
                                   frequencies
                                   (sort (fn [a b] (compare (second a) (second b)))))]
                    freqs
                    (-  (second (last freqs)) (second (first freqs))))))

  (polymer-process (parse-polymerization example) 10 rating-1)
;; => 1588

  (polymer-process (parse-polymerization (slurp (io/resource "2021/day-14"))) 10 rating-1)
;; => 2584

;; Part 2

  (def rating-2 (fn [state]
                  (let [freqs (->> state
                                   (mapcat (fn [[seg freq]] [[(first seg) freq] [(second seg) freq]]))
                                   (reduce (fn [res [k v]] (update res k #(if % (+ % v) v))) {})
                                   (map (fn [[k v]] [k (long (+ (/ v 2) 0.5))]))
                                   (into {})
                                   (sort (fn [a b] (compare (second a) (second b)))))]
                    (-  (second (last freqs)) (second (first freqs))))))

  (polymer-process-faster (parse-polymerization example) 40 rating-2)
;; => 2188189693529


  (polymer-process-faster (parse-polymerization (slurp (io/resource "2021/day-14"))) 40 rating-2)
;; => 3816397135460
  )
