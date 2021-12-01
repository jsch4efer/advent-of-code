(ns advent-of-code.2020.day-7
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn parse-bag-rules [input]
  (->> input
       string/split-lines
       (map #(re-find #"^(\w+ \w+) bags contain (.*)\.$" %))
       (map (fn [[_ bag content]]
              [bag (->> (string/split content #", ")
                        (map #(re-find #"^(\d+) (\w+ \w+) bags?$" %))
                        (map (fn [[_ quantity sub-bag]]
                               (when (and quantity sub-bag)
                                 [sub-bag (read-string quantity)])))
                        (into {}))]))

       (into {})))

(defn- source-nodes-for [graph target]
  (->> graph (filter (fn [[s es]] (get es target))) (map first) (into #{})))

(defn find-all-outermost-bags-for [bag-rules bag]
  (loop [candidates (source-nodes-for bag-rules bag)
         result candidates]
    (if-not (empty? candidates)
      (let [next-candidates (->> candidates
                                 (mapcat #(source-nodes-for bag-rules %))
                                 (filter #(not (contains? result %)))
                                 (into #{}))]
        (recur next-candidates (clojure.set/union result candidates)))
      result)))

(def example-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
")

(comment

  ;; Part 1


  (find-all-outermost-bags-for (parse-bag-rules example-input) "shiny gold")
  ;; => #{"muted yellow" "light red" "dark orange" "bright white"}

  (-> (find-all-outermost-bags-for (parse-bag-rules (slurp (io/resource "2020/day-7"))) "shiny gold")
      count)
  ;; => 224
)

(defn- transform-bag-rules [bag-rules]
  (->> bag-rules
       (map (fn [[b c]] [b (->> c (map (fn [[a b]] [b a])) (into []))]))
       (map (fn [[b c]] [b (if (empty? c) 0 c)]))
       (into {})))

(defn- reduced-bags [bag-sizes]
  (->> bag-sizes
       (filter #(number? (second %)))
       (into {})))

(defn- all-reducible? [content]
  (->> content
       (map (fn [entry] (and (coll? entry) (number? (first entry)) (number? (second entry)))))
       (reduce (fn [res next] (and res next)) true)))

(defn- all-reduced? [content]
  (->> content
       (map number?)
       (reduce (fn [res next] (and res next)) true)))

(defn- update-size [bag-sizes reduced]
  (->> bag-sizes
       (map (fn [[b c]]
              [b
               (if (vector? c)
                 (if (all-reduced? c)
                   (reduce + c)
                   (if (all-reducible? c)
                     (->> c
                          (map (fn [[cnt size]] (+ cnt (* cnt size))))
                          (into []))
                     (->> c
                          (map (fn [d]
                                 (if (number? d)
                                   d
                                   (let [[cnt sb] d]
                                     [cnt (get reduced sb sb)]))))
                          (into []))))
                 c)]))
       (into {})))

(defn calculate-size [bag-rules bag]
  (when (get bag-rules bag)
    (loop [bag-sizes (transform-bag-rules bag-rules)]
      (let [reduced (reduced-bags bag-sizes)]
        (if-let [result (get reduced bag)]
          result
          (recur (update-size bag-sizes reduced)))))))

(comment

  ;; Part 2

  (calculate-size (parse-bag-rules example-input) "shiny gold")
  ;; => 32

  (calculate-size (parse-bag-rules (slurp (io/resource "2020/day-7"))) "shiny gold")
  ;; => 1488
)
