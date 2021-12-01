(ns advent-of-code.input
  (:require [clj-http.client :as client]))

(defn get-input [day]
  (client/get (str "https://adventofcode.com/2020/day/" day "/input")))

(comment
  (get-input

   1))
