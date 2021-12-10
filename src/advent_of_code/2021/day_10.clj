(ns advent-of-code.2021.day-10
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn opposite-char [char]
  (case char
    \) \(
    \(  \)
    \] \[
    \[  \]
    \> \<
    \<  \>
    \{  \}
    \} \{
    nil))

(defn is-closing [char]
  (contains? #{\) \] \> \}} char))

(defn is-opening [char]
  (contains? #{\( \[ \< \{} char))

(defn give-points [errors]
  (->> errors
       (map first)
       frequencies
       (map #(* (case %1
                  \) 3
                  \] 57
                  \} 1197
                  \> 25137)
                %2))))

(defn validate-line [line]
  (let [max-index (- (count line) 1)]
    (loop [stack '()
           index 0]
      (if-let [top (peek stack)]
        (if (<= index max-index)
          (let [next (nth line index)]
            (if (is-opening next)
              (recur (conj stack next) (inc index))
              (let [expected (opposite-char top)]
                (if (= next expected)
                  (recur (pop stack) (inc index))
                  {:status :invalid
                   :expected expected
                   :actual next
                   :stack stack
                   :pos index
                   :in line
                   :branch 1}))))
          {:status :incomplete
           :expected (opposite-char (peek (pop stack)))
           :actual top
           :stack stack
           :pos index
           :in line
           :branch 2})
        (if (<= index max-index)
          (let [next (nth line index)]
            (if (is-opening next)
              (recur (conj stack (nth line index)) (inc index))
              {:status :invalid
               :expected (opposite-char next)
               :actual next
               :stack stack
               :pos index
               :in line
               :branch 3}))

          {:status :valid
           :in line})))))

(defn parse-input [input]
  (string/split-lines (string/replace input #" " "")))

(defn validate [input]
  (->> (parse-input input)
       (map validate-line)))

(comment

  (def example-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
")

  (def input (slurp (io/resource "2021/day-10")))

  (defn evaluation-part1 [validations]
    (->> validations
         (filter #(-> % :status (= :invalid)))
         (map :actual)
         (frequencies)
         (map (fn [[k v]]
                (* v (case k
                       \) 3
                       \] 57
                       \} 1197
                       \> 25137
                       0))))
         (reduce +)))

    ;; Part 1


  (evaluation-part1 (validate example-input))
;; => 26397

  (evaluation-part1 (validate (slurp (io/resource "2021/day-10"))))
;; => 387363

  ;; Part 2

  (defn evaluation-part2 [validations]
    (println "Evaluation part 2")
    (let [scores (->> validations
                      (filter #(-> % :status (= :incomplete)))
                      (map :stack)
                      (map (partial map opposite-char))
                      (map (partial map {\) 1 \] 2 \} 3 \> 4}))
                      (map (partial reduce (fn [r v] (+ (* 5 r) v))))
                      (sort))]
      (nth scores (/ (count scores) 2))))

  (evaluation-part2 (validate example-input))
;; => 288957

  (evaluation-part2 (validate (slurp (io/resource "2021/day-10"))))
;; => 4330777059
  )
