(ns advent-of-code.2021.day-2
  (:require [clojure.string :as string]
            [advent-of-code.input :as input]))

(defn parse-submarine-commands [input]
  (->> input
       (string/split-lines)
       (map #(string/split  % #" "))
       (map (fn [[cmd param]] [(keyword cmd) (read-string param)]))))

(defn run-submarine [init-state interpretation commands]
  (let [final-state (->> commands
                         (reduce (fn [state cmd]
                                   ((interpretation cmd) state))
                                 init-state))]

    (* (get final-state :horizontal 0) (get final-state :depth 0))))

(comment
  ;; Part 1


  (defn part-1-interpreation [[action val]]
    (case action
      :forward (fn [s] (update s :horizontal + val))
      :up  (fn [s] (update s :depth  + (* -1 val)))
      :down (fn [s] (update s :depth + val))))

  (->> "forward 5
down 5
forward 8
up 3
down 8
forward 2
"
       (parse-submarine-commands)
       (run-submarine
        {:depth 0 :horizontal 0}
        part-1-interpreation))

;; => 150


  (->> (input/read-input! "2021/day-2")
       (parse-submarine-commands)
       (run-submarine
        {:depth 0 :horizontal 0 :aim 0}
        part-1-interpreation))
;; => 2147104


  ;; Part 2

  (defn part-2-interpretation  [[cmd val]]
    (case cmd
      :forward (fn [s] (-> s
                           (update :horizontal + val)
                           (update :depth + (* (:aim s) val))))
      :up (fn [s] (update s :aim + (* -1 val)))
      :down (fn [s] (update s :aim + val))))

  (->> "forward 5
down 5
forward 8
up 3
down 8
forward 2
"
       (parse-submarine-commands)
       (run-submarine
        {:depth 0 :horizontal 0 :aim 0}
        part-2-interpretation))
;; => 900

  (->> (input/read-input! "2021/day-2")
       (parse-submarine-commands)
       (run-submarine
        {:depth 0 :horizontal 0 :aim 0}
        part-2-interpretation))
;; => 2044620088
  )
