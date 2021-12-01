(ns advent-of-code.input
  (:require [clj-http.client :as client]))

(defn get-input [year day]
  (client/get (str "https://adventofcode.com/" year "/day/" day "/input")))

(comment
  (get-input
   2021
   1))
