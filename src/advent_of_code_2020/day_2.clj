(ns advent-of-code-2020.day-2
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))




;; Part 1


(defn valid-passwords-policy1 [data]
  (let [entries (s/split-lines data)
        valid-entries (->> entries
                           (map (fn [l]
                                  (let [[rng car pass] (s/split l #" ")]
                                    {:range (->> (s/split rng #"-") (map read-string))
                                     :char (first (s/replace car ":" ""))
                                     :pass pass})))

                           (filter (fn [{:keys [rng ch pass]}]
                                     (<= (first rng) (or ((frequencies pass) ch) -1) (last rng)))))]
    (count valid-entries)))

(comment

  (valid-passwords-policy1 "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")
  ;; => 2


  (valid-passwords-policy1 (slurp (io/resource "day-2")))
  ;; => 410
)


;; Part 2


(defn get-pos-map [s]
  (->> s
       (map-indexed (fn [index ch] [ch (inc index)]))
       (reduce (fn [res [ch index]]
                 (update res ch (fn [old]
                                  (conj (or old #{}) index))))
               {})))

(defn valid-passwords-policy2 [data]
  (let [entries (s/split-lines data)
        valid-entries (->> entries
                           (map (fn [l]
                                  (let [[rng car pass] (s/split l #" ")]
                                    {:pos (->> (s/split rng #"-") (map read-string) (into []))
                                     :ch (first (s/replace car ":" ""))
                                     :pass pass})))
                           (filter (fn [{:keys [pos ch pass]}]
                                     (let [pass-positions (get-pos-map pass)
                                           first-pos (first pos)
                                           second-pos (second pos)]
                                       (= 1
                                          (cond->> 0
                                            (contains? (pass-positions ch) first-pos) inc
                                            (contains? (pass-positions ch) second-pos) inc))))))]
    (count valid-entries)))

(comment

  (valid-passwords-policy2 "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")
  ;; => 1

  (valid-passwords-policy2 (slurp (io/resource "day-2")))
  ;; => 694
)
