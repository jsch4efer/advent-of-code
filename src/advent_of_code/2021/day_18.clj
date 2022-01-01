(ns advent-of-code.2021.day-18
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn parse-sf
  ([base-key input]
   (->> input
        (map-indexed (fn [i e]
                       (merge (if (coll? e)
                                (parse-sf (str base-key i) e)
                                {})
                              {(str base-key i) e})))

        (reduce merge {})))
  ([input]
   (parse-sf "" input)))

(defn sf-add [sf1 sf2]
  (cond
    (and sf1 sf2) [sf1 sf2]
    sf1 sf1
    sf2 sf2))

(defn index-to-list [index]
  (->> (seq index) (map #(Integer/parseInt (str %))) (into [])))

(defn left-neighbor [sf pos]
  (let [pos-index (string/join pos)
        left (->> (parse-sf sf)
                  (filter #(number? (second %)))
                  (sort-by first)
                  (split-with #(not= (first %) pos-index))
                  (first)
                  (last))]
    (when left
      [(index-to-list (first left)) (second left)])))

(defn right-neighbor [sf pos]
  (let [pos-index (string/join  pos)
        right (->> (parse-sf sf)
                   (filter #(number? (second %)))
                   (sort-by first)
                   (split-with #(not= (first %) pos-index))
                   (second)
                   (second))]
    (when right
      [(index-to-list (first right)) (second right)])))

(defn explode-pair [sf p]
  (let [[l r] (get-in sf p)
        ln (left-neighbor sf (conj p 0))
        rn (right-neighbor sf (conj p 1))]
    (cond-> sf
      ln (update-in (first ln) (partial + l))
      rn (update-in (first rn) (partial + r))
      true (assoc-in p 0))))

(defn split-pair [sf p]
  (update-in sf p
             (fn [old]
               [(int (Math/floor (/ old 2)))
                (int (Math/ceil  (/ old 2)))])))

(defn sf-reduce [sf]
  (let [parsed (parse-sf sf)
        to-explode (->> parsed (filter #(and (coll? (second %)) (= 4 (count (first %))))) (sort-by first) first)
        to-split (->> parsed (filter #(and (number? (second %)) (<= 10 (second %)))) (sort-by first) first)]
    (cond
      to-explode (sf-reduce (explode-pair sf (index-to-list (first to-explode))))
      to-split (sf-reduce (split-pair sf (index-to-list (first to-split))))
      :else sf)))

(defn sf-magnitude  [sf]

  (cond
    (coll? sf) (+ (* 3 (sf-magnitude (first sf)))
                  (* 2 (sf-magnitude (second sf))))
    (number? sf) sf))

(defn magnitude-of-all-added [sfs]
  (sf-magnitude (reduce #(sf-reduce (sf-add %1 %2)) sfs)))

(defn maximized-magnitude-of-pairs [sfs]
  (->> (for [left sfs
             right sfs]
         [left right])
       (filter (partial apply not=))
       (pmap (comp sf-magnitude sf-reduce (partial apply sf-add)))
       sort
       last))

(defn parse-homework [input]
  (->> (string/split-lines input)
       (map read-string)))

(comment

  (def ex  [0 [1 2] 3 [4 [5 6]]])

  (left-neighbor ex [3 1 0])

  (right-neighbor ex [2])

  (sf-add 0 [1 3])

  (explode-pair [[[[[9,8],1],2],3],4] [0 0 0 0])
;; => [[[[0 9] 2] 3] 4]

  (explode-pair [7,[6,[5,[4,[3,2]]]]] [1 1 1 1])
;; => [7 [6 [5 [7 0]]]]

  (explode-pair [[6,[5,[4,[3,2]]]],1] [0 1 1 1])
;; => [[6 [5 [7 0]]] 3]

  (split-pair [11] [0])
;; => [[5 6]]

  (sf-reduce (sf-add [[[[4,3],4],4],[7,[[8,4],9]]] [1 1]))
;; => [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]

  (sf-magnitude [[1,2],[[3,4],5]])
;; => 143

  (sf-reduce [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]])
;; => [[[0 [5 8]] [[1 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]]



  ;; Part 1


  (def example "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
")

  (magnitude-of-all-added (parse-homework example))
;; => 4140

  (magnitude-of-all-added (parse-homework (slurp (io/resource "2021/day-18"))))
;; => 4202

  ;; Part 2
  (maximized-magnitude-of-pairs (parse-homework example))

  (maximized-magnitude-of-pairs (parse-homework (slurp (io/resource "2021/day-18"))))
;; => 4779
  )
