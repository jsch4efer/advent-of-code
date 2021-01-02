(ns advent-of-code-2020.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.set :as cs]))



(defn parse-passports [data]
  (->> data
       string/split-lines
       (mapcat #(string/split % #"\s"))
       (partition-by (partial = ""))
       (filter #(not= % '("")))
       (map (fn [passport] (->> passport
                                (map (fn [e]
                                       (let [[k v] (string/split e #":")]
                                         [(keyword k) v])))
                                (into {}))))
       ))

(defn validate-passports-part-1 [passports]
  (let [results (->> passports
                    (reduce (fn [report passport]
                              (let [valid? (cs/subset?  #{:eyr :iyr :byr :pid :hcl :hgt :ecl} (into #{} (keys passport)))
                                    ]
                                (update report (if valid? :valid :invalid) #(conj % passport))))
                            {:valid []
                             :invalid []}))]
    {:summary {:valid (->> results :valid count)
               :invalid (->> results :invalid count)}
     :passports results
     }
    ))

(def example-data "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")


;; Part 1

(comment
  (->> (parse-passports example-data)
       validate-passports-part-1
       :summary
       )
  ;; => {:valid 2, :invalid 2}


  (->> (slurp (io/resource "day-4"))
       parse-passports
       validate-passports-part-1
       :summary
       )
  ;; => {:valid 219, :invalid 68}

  )


;; Part 2

(s/def ::byr (s/and #(re-find #"^\w{4}$" %) #(<= 1920 (read-string %) 2002)))
(s/def ::iyr (s/and #(re-find #"^\w{4}$" %) #(<= 2010 (read-string %) 2020)))
(s/def ::eyr (s/and #(re-find #"^\w{4}$" %) #(<= 2020 (read-string %) 2030)))
(s/def ::hgt (s/and #(re-find #"^\d+(cm|in)$" %) #(let [[_ height unit] (re-find #"^(\d+)(cm|in)$" %)]
                                                    (case unit
                                                      "cm" (<= 150 (read-string height) 193)
                                                      "in" (<= 59 (read-string height) 76))
                                                    )))
(s/def ::hcl #(re-find #"^#[a-f0-9]{6}$" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-find #"^\d{9}$" %))
(s/def ::cid (constantly true))

(s/def ::passport (s/keys :req-un [::eyr ::iyr ::byr ::pid ::hcl ::hgt ::ecl]
                          :opt-un [::cid]))

(defn validate-passports-part-2 [passports]
  (let [results (->> passports
                     (reduce (fn [report passport]
                               (let [valid? (s/valid? ::passport passport)
                                     output (when-not valid? (with-out-str (s/explain ::passport passport)))]
                                 (update report (if valid? :valid :invalid) #(conj % [passport output]))))
                             {:valid []
                              :invalid []}))]
    {:summary {:valid (->> results :valid count)
               :invalid (->> results :invalid count)}
     :passports results
     }
    ))



(comment

  (def invalid-examples "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")


  (def valid-examples "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" )

  (->> invalid-examples
       parse-passports
       validate-passports-part-2
       :summary)
  ;; => {:valid 0, :invalid 4}

  (->> valid-examples
       parse-passports
       validate-passports-part-2
       :summary)
  ;; => {:valid 4, :invalid 0}


  (->> (slurp (io/resource "day-4"))
       parse-passports
       validate-passports-part-2
       :summary)
  ;; => {:valid 127, :invalid 160}

  )
