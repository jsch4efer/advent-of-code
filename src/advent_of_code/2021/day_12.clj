(ns advent-of-code.2021.day-12
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-caves [input]
  (->> (string/split-lines input)
       (mapcat (fn [line]
                 (let [[left right] (->> (string/split line #"-")
                                         (map keyword)
                                         (into []))]
                   [[left right] [right left]])))
       (filter #(not (or (= :end (first %)) (= :start (second %)))))
       (reduce (fn [res [k v]]
                 (update res k #(conj %1 v))) {})))

(defn find-cave-paths [cave]
  (loop [paths '(((:start)))]
    (let [paths-n-1 (first paths)
          paths-n (reduce (fn [pn pn-1]
                            (let [s (first pn-1)
                                  psn (->> (get cave s '())
                                           (filter #(or (not-any? (partial = %) pn-1) (re-matches #"[A-Z]+" (name %))))
                                           (map (partial conj pn-1))
                                           (into '()))]
                              (concat pn psn)))
                          '()
                          paths-n-1)]
      (if (empty? paths-n)
        (->>  paths
              (mapcat identity)
              (map reverse)
              (filter #(and (= (first %) :start) (= (last %) :end)))
              (into '()))
        (recur (conj paths paths-n))))))

(defn find-cave-paths-2 [cave]
  (let [big-cave? (fn [cave] (re-matches #"[A-Z]+" (name cave)))
        extend-path (fn [[p t s] c]
                      (cond
                        (big-cave? c)
                        [(conj p c) t s]
                        (not (s c))
                        [(conj p c) t (conj s c)]
                        (nil? t)
                        [(conj p c) c s]))]

    (loop [paths '(([(:start) nil #{}]))
           n 1]
      (let [paths-n-1 (first paths)]
        (let [paths-n (->> paths-n-1
                           (mapcat (fn [[p t s]]
                                     (when-let [last-cave (first p)]
                                       (->> (for [n (get cave last-cave)]
                                              (extend-path [p t s] n))
                                            (filter identity)
                                            (into '())))))
                           (filter identity)
                           (into '()))]
          (if (empty? paths-n)
            (->>  paths
                  (mapcat identity)
                  (map first)
                  (filter #(and (= (first %) :end) (= (last %) :start)))
                  (into '()))
            (recur (conj paths paths-n) (inc n))))))))

(comment

  (def first-example  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

  (def second-example "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
")

  (def third-example "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
")

  (def input "ax-end
xq-GF
end-xq
im-wg
ax-ie
start-ws
ie-ws
CV-start
ng-wg
ng-ie
GF-ng
ng-av
CV-end
ie-GF
CV-ie
im-xq
start-GF
GF-ws
wg-LY
CV-ws
im-CV
CV-wg
")

  (-> (find-cave-paths (parse-caves first-example)) count)
;; => 10
  (-> (find-cave-paths (parse-caves second-example)) count)
;; => 19


  (-> (find-cave-paths (parse-caves third-example)) count)
;; => 226


  (-> (find-cave-paths (parse-caves input)) count)
;; => 3576


  ;; Part 2

  (-> (find-cave-paths-2 (parse-caves first-example)) count)
;; => 36

  (-> (find-cave-paths-2 (parse-caves input)) count)
;; => 84271
  )
