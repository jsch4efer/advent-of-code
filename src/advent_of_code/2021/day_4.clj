(ns advent-of-code.2021.day-4
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))



(def example-bingo "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(defn parse-bingo [input]
  (let [lines (->> input string/split-lines (map string/trim))
        choices (-> lines first (string/split #","))
        boards (->> lines
                      (drop 1)
                      (map (fn [line] (->> (string/split line #"\s+")
                                           (into []))))
                      (reduce (fn [[boards current] line]
                                (if (= 1 (count line))
                                  (if current
                                    [(conj boards current) []]
                                    [boards []])
                                  [boards (conj current line)]
                                  )
                                )
                              [[] nil])
                      (apply (partial conj))
                       )
        ]
    {:choices choices
     :boards boards
     }
    )

  )

(defn is-board-finished? [board]
  (let [down-diag (->> (range 5)
                       (map #(get-in board [% %] ))
                       (into []))
        up-diag (->> (range 5)
                     (map #(get-in board [(- 4 %) %] ))
                     (into []))]
    (->>
       (concat board (apply map vector board) [down-diag] [up-diag])
       (filter #(every? (partial = "x") %))
       (first)
       (some?))

    )
  )

(comment
  (every? (partial = "x") ["x" "1"])

  (is-board-finished? [["x" "x" "x" "x" "x"]
                       [1 1 1 1 1]
                       [2 2 2 2 2]
                       [3 3 3 3 3]
                       [4 4 4 4 4]])

  )

(defn score-board [board choices]
  (->> (apply concat board)
       (filter (partial not= "x"))
       (map read-string)
       (reduce +)
       (*  (read-string (last choices)))
       )
  )

(defn run-bingo [bingo]
  (loop [boards (:boards bingo)
         considered []
         remained (:choices bingo)]
    (println "Considered:" considered "Remained:" remained "Boards:" boards)
    (if-let [winner (first (filter is-board-finished? boards))]
      (score-board winner considered)
      (if (empty? remained)
        "No winner"
        (let [choice (first remained)
              new-boards (for [board boards]
                           (for [row board]
                             (replace {choice "x"} row)))
              ]
          (recur new-boards (conj considered choice) (rest remained))
          )))
    ))

(comment


 ;; Part 1

  (run-bingo (parse-bingo example-bingo))
;; => 4512

  (run-bingo (parse-bingo (-> (io/resource "2021/day-4") slurp)))
;; => 55770


  ;; Part 2




  )
