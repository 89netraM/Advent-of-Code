(require
  '[clojure.java.io :as io]
  '[clojure.string :as str])

(defn parse-ranges [ranges]
  (map (fn [range]
    (let [[from to] (map Long/parseLong (str/split range #"-" 2))]
      {:from from :to to})) ranges))

(defn is-in-range [subject {from :from to :to}]
  (and (<= from subject) (<= subject to)))

(defn is-in-ranges [ranges subject]
  (some (partial is-in-range subject) ranges))

(defn is-overlapping [a b]
  (or (is-in-range (a :from) b) (is-in-range (a :to) b) (is-in-range (b :from) a) (is-in-range (b :to) a)))

(defn merge-range [a list]
  (if (empty? list)
    nil
    (let [[b & rest] list]
      (if (is-overlapping a b)
        (conj rest {:from (min (a :from) (b :from)) :to (max (a :to) (b :to))})
        (let [merged-ranges (merge-range a (vec rest))]
          (if merged-ranges
            (conj merged-ranges b)
            nil))))))

(defn compact-ranges [ranges]
  (if (<= (count ranges) 1)
    ranges
    (let [[first & rest] ranges
        compacted (compact-ranges (vec rest))
        merged (merge-range first compacted)]
      (if merged
        (compact-ranges merged)
        (conj compacted first)))))

(defn sum-range [{from :from to :to}]
  (+ 1 (- to from)))

(let [[ranges ingredients] (map str/split-lines (str/split (slurp "input.txt") #"\r\n\r\n" 2))
    ranges (compact-ranges (parse-ranges ranges))
    ingredients (map Long/parseLong ingredients)]
  (println (str "Part 1: " (count (filter (partial is-in-ranges ranges) ingredients))))
  (println (str "Part 2: " (reduce + (map sum-range ranges)))))
