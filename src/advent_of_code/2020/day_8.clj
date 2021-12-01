(ns advent-of-code.2020.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def example-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-input [input]
  (->> (string/split-lines input)
       (map (fn [cmd]
              (let [[_ op value]  (re-find #"^(\w+) ([\+-]\d+)$" cmd)]
                {:op (keyword op)
                 :value (read-string value)})))
       (into [])))

;; Part 1

(defn detect-infinite-loop [program]
  (loop [pc 0
         acc 0
         visited-cmds #{}]
    (let [{:keys [op value]} (nth program pc)]
      (if (or  (visited-cmds pc)
               (>= pc (count program)))
        {:acc acc
         :regular-termination (= pc (count program))}
        (let [next-visited-cmds (conj visited-cmds pc)
              [next-pc next-acc] (case op
                                   :acc [(inc pc) (+ acc value)]
                                   :jmp [(+ pc value) acc]
                                   :nop [(inc pc) acc])]
          (recur next-pc next-acc next-visited-cmds))))))

(comment

  (-> (parse-input example-input)
      detect-infinite-loop
      :acc)
  ;; => 5


  (-> (slurp (io/resource "day-8"))
      parse-input
      detect-infinite-loop
      :acc)
  ;; => 1930
)


;; Part 2

(defn detect-and-record-infinite-loop [program]
  (loop [pc 0
         acc 0
         visited-cmds #{}
         trace []]
    (let [{:keys [op value]} (nth program pc {:op nil :value nil})]
      (if (or  (visited-cmds pc)
               (>= pc (count program)))
        {:acc acc
         :regular-termination (>= pc (count program))
         :trace trace}
        (let [next-visited-cmds (conj visited-cmds pc)
              [next-pc next-acc] (case op
                                   :acc [(inc pc) (+ acc value)]
                                   :jmp [(+ pc value) acc]
                                   :nop [(inc pc) acc])]

          (recur next-pc next-acc next-visited-cmds (conj trace pc)))))))

(defn fix-looping-program [program]
  (let [ last-execution (detect-and-record-infinite-loop program)
        {:keys [acc regular-termination trace]} last-execution]
    (if regular-termination
      acc
      (->> (reverse trace)
           (filter #(= (:op (nth program %)) :jmp))
           (map #(detect-and-record-infinite-loop (update program % assoc :op :nop)))
           (filter :regular-termination)
           (map :acc)
           (first))
      )
    ))


(comment
  (fix-looping-program (parse-input example-input))
  ;; => 8

  (fix-looping-program (parse-input (slurp (io/resource "day-8"))))
  ;; => 1688
  )
