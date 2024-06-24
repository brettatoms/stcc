(ns challenge.core
  (:require [clojure.set :as set])
  (:import (java.time LocalDate Period)
           (org.threeten.extra LocalDateRange)))

(defn iter-dt-range
  "Create an iterator over the days of a LocalDateRange"
  [^LocalDateRange dt-range]
  (iterator-seq (.iterator (.stream dt-range))))

(defn unique-days
  "Given a seq of LocalDateRange instances return unique set of the days from all the ranges."
  [dt-ranges]
  (reduce (fn [acc cur]
            (set/union acc (set (iter-dt-range cur))))
          #{}
          dt-ranges))

(defn extend-dt-range
  "Extend a LocalDateRange by a number of days."
  [^LocalDateRange dt-range ^long ndays]
  (.withEnd dt-range (.plusDays (.getEnd dt-range) ndays)))

(defn ends-on?
  "Returns true if the LocalDateRanges on on dt."
  [^LocalDateRange dt-range ^LocalDate dt]
  (= (.getEnd dt-range) dt))

(defn dt-range
  "Create a new LocalDateRange."
  ([^LocalDate start]
   (dt-range start (Period/ofDays 1)))
  ([^LocalDate start ^Period period]
   (LocalDateRange/of start period)))

;; TODO: This function doesn't work with unbounded date ranges b/c we calculate
;; the diff by getting the unique days from each date range first.
(defn difference
  "Return a set of data ranges with where the days included in (rest input) are remove from (first input)."
  [& input]
  (let [[base & sub] input
        base-days (unique-days base)
        sub (apply set/union sub)
        sub-days (unique-days sub)
        ;; days is base-days with all of sub-days removed
        days (sort (set/difference base-days sub-days))]
    (if (seq days)
      (set (reduce (fn [[first & rest :as acc] cur]
                     (if (ends-on? first cur)
                       ;; the end is the same as the current date so extend
                       ;; the range by a day
                       (cons (extend-dt-range first 1) rest)
                       ;; this end isn't the same as the current date so add a
                       ;; new range to the acc
                       (cons (dt-range cur) acc)))
                   [(dt-range (first days))]
                   (rest days)))
      ;; No unique days after removing subdays so return the empty set.
      #{})))
