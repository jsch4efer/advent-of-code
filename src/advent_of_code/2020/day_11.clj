(ns advent-of-code.2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def example "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn- extend-waiting-room [waiting-room]
  (let [horizontal-extension (into [] (repeat (count (first waiting-room)) 0))]
    (->> (concat [horizontal-extension] waiting-room [horizontal-extension])
         (map #(into [] (concat [0] % [0])) )
         (into []))))

(defn- occupied-seats-near-by [waiting-room [pos-x pos-y]]
  (let [correction-term (case (get-in waiting-room [pos-y pos-x])
                          1 1
                          0)]
    (->> (for [x (range (dec pos-x) (+ pos-x 2))
               y (range (dec pos-y) (+ pos-y 2))]
           (get-in waiting-room [y x]))
         (filter (partial = 1))
         (reduce (fn [res _] (inc res)))
         (apply #(- % correction-term))
         )))

(defn- simulate-waiting-room-step [waiting-room]
  (let [extended-waiting-room (extend-waiting-room waiting-room)]
    (->> waiting-room
         (map-indexed
          (fn [y row]
            (->> (map-indexed (fn [x seat]
                                    (let [occupied (occupied-seats-near-by extended-waiting-room [(inc x) (inc y)])]
                                      (cond
                                        (and (= seat 0) (= occupied 0)) 1
                                        (and (= seat 1) (<= 4 occupied)) 0
                                        :else seat)))
                              row)
                 (into []))))
         (into []))))

(defn- count-occupied-seats [waiting-room]
  (->> waiting-room
       (mapcat identity)
       (reduce (fn [res next] (if (= next 1) (inc res) res)) 0))
  )

(defn- print-and-pass [round x]
  (println "Round" round (count-occupied-seats x))
    x)


(comment
  (let [parsed (->> example
                    string/split-lines
                    (map (fn [line]
                           (->> line
                                (map #(case % \L 0 \. nil \# 1) )
                                (into []))))
                    (into []))]
    (->> parsed
         simulate-waiting-room-step
         ;; (print-and-pass 1)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 2)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 3)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 4)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 5)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 6)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 7)
         ;; simulate-waiting-room-step
         ;; (print-and-pass 8)

         ))


  )

#_(loop [current-waiting-room parsed
            steps 0]
       (let [next-waiting-room (simulate-waiting-room-step current-waiting-room)]
         (if (= current-waiting-room next-waiting-room)
           steps
           (recur next-waiting-room (inc steps)))
         ))
