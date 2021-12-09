(ns advent-of-code.2021.day-8
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))


(defn digits-by-count [config]
  (reduce-kv (fn [res k _]
               (update res
                       (count k)
                       (fn [old]
                         (if old
                           (conj old k)
                           [k]))))

             {}
             config))

(defn parse-digit [input]
  (string/split (string/trim input) #"\s"))


(defn decode-output [config output]
  (->> output
       (map #(apply hash-set (seq %)))
       (map (partial config))
       (filter identity)))

(defn parse-notes [input]
  (->> (string/split-lines input)
       (mapcat (fn [line]
                 (->> (string/split line #"\|")
                      (map string/trim)
                      (map #(string/split % #"\s"))
                      )))
       (partition 2)
       (map vec)
       (into [])))

(defn to-known-numbers-part-1 [training-fn input]
  (->> (parse-notes input)
       (map (fn [[signal-patterns output]]
              (decode-output (training-fn signal-patterns) output)
               ))
       (filter not-empty)
       ))

(defn learn-config-from [config]
  (fn [signal-patterns]
    (->> signal-patterns
         (map (fn [signal-pattern]
                (let [digits ((digits-by-count config) (count signal-pattern))
                      digit (apply hash-set (seq signal-pattern))]
                  (when (= (count digits) 1)
                    [digit (config (first digits))]
                    ))))
         (filter identity)
         (reduce conj {}))))


(comment


  ;; Part 1
  ;;
  (def config-part1
    {#{\a \b \c \e \f \g} 0
     #{\c \f} 1
     #{\a \c \d \e \g} 2
     #{\a \c \d \f \g} 3
     #{\b \c \d \f} 4
     #{\a \b \d \f \g} 5
     #{\a \b \d \e \f} 6
     #{\a \c \f} 7
     #{\a \b \c \d \e \f \g} 8
     #{\a \b \c \d \f \g} 9})

  (def example-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
cdfeb fcadb cdfeb cdbaf")

  (def example-input-2 "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
fgae cfgab fg bagce
")


  (->> (to-known-numbers-part-1 (learn-config-from config-part1) example-input-2)
       (mapcat identity)
       (count)
       )
;; => 26

  (def input (slurp (io/resource "2021/day-8")))

  (->> (to-known-numbers-part-1 (learn-config-from  config-part1) input)
       (mapcat identity)
       count)
;; => 342

  ;; Part 2

  (def config-part2
    {#{\a \b \c \e \d \g} 0
     #{\a \b} 1
     #{\a \c \d \f \g} 2
     #{\a \c \d \f \b} 3
     #{\a \e \b \f} 4
     #{\b \d \e \f \c} 5
     #{\b \d \e \f \c \g} 6
     #{\a \b \d} 7
     #{\a \b \c \d \e \f \g} 8
     #{\a \b \c \d \e \f} 9})


  (->> (to-known-numbers-part-1 (fn [s]
                                  (let [learned ((learn-config-from config-part2) s)
                                        learned-first ((learn-config-from config-part1) s)]
                                    (fn [digit]
                                      (let [one (learned digit)
                                            two (learned-first digit)
                                            three (config-part1 digit)
                                            four (config-part2 digit)]
                                        (println "one" one "two" two "three" three "four" four)
                                        (or
                                         one
                                         two
                                         three
                                         four
                                         ))
                                      )

                                    )) input)
       (map (partial reduce (fn [res next] (+ (* 10 res) next)) 0))
       (reduce +)
       )

  (println "FOo")





  )
