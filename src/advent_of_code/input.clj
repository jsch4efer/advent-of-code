(ns advent-of-code.input
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn get-input [year day]
  (client/get (str "https://adventofcode.com/" year "/day/" day "/input")))


(defn read-input! [location]
  (->> (slurp (io/resource location))))

(comment
  (get-input
   2021
   1))
