(ns clojure-stat.core)

(defn- verify-stats-input-for-summarize [stats]
  "This is a private helper function for the summarize macro.
   If data has three elements and the first element is not a vector, then wrap the three elements in a vector,
   and nest that vector in another vector. Otherwise, return the stats vector as is.
  "
  (cond
    (and (= (count stats) 3) (not (vector? (first stats)))) (vector stats)
    :else stats 
    )
  )

(defmacro ^{:private true} summarize
  "Takes a vector of maps (data), a vector of keys (groups), and a vector of statistics (stats).  The function applies the summary stats in (stats) to
   each partition that is created from the keys in (groups).
   Sample data and macro call proceed:
   (def sample_data [
          {:dt 20110331 :name \"John\" :age 22 :salary 65000 :education 12}
          {:dt 20120331 :name \"John\" :age 23 :salary 65000 :education 12}
          {:dt 20110331 :name \"Jane\" :age 22 :salary 100000 :education 16}
          {:dt 20120331 :name \"Jane\" :age 23 :salary 101000 :education 16}
          {:dt 20110331 :name \"Eric\" :age 22 :salary 25000 :education 12}
          {:dt 20120331 :name \"Eric\" :age 23 :salary 25000 :education 12}
          ])
   The following will summarize the data by :dt and :education. The macro will apply the mean function to :salary and 
   place the result in a key named avg_salary.  The macro will apply the + function to :salary and place the result
   in a key named aggregate_income
   (summarize sample_data [:dt :education] [[:salary mean \"avg_salary\"] [:salary + \"aggregate_income\"]])
   other examples:   
   (summarize sample_data [:education :age] [[:salary + \"aggregate_income\"]])
   (summarize sample_data [:education :age] [:salary + \"aggregate_income\"])
   (summarize sample_data [[:salary + \"aggregate_income\"]])
   (summarize sample_data [:salary + \"aggregate_income\"])
  "
  ([data group stats]
   `(let [sorted-data# (sort-by (juxt ~@group) ~data)
          partitioned-data# (partition-by #(apply vector (map (fn [attr#] (attr# %)) ~group)) sorted-data#)]
      (map
       (fn [partition#]
         (let [first_record# (first partition#)
               grouping# (reduce (fn [result# obs#] (assoc result# obs# (obs# first_record#))) {} ~group)
               stats# (clojure-stat.core/summarize partition# ~stats)]
           (merge grouping# stats#)
           )
        )
       partitioned-data#
       )
      )
   )
   ([data stats]
    `(let [
           verified-stats# (verify-stats-input-for-summarize ~stats)
           stats# (reduce (fn [result# [item# fn# name#]] (conj result# (keyword name#) (apply fn# (map item# ~data)))) [] verified-stats#)
           ]
       (apply hash-map stats#)
       )
     )
  )

(defn mean [& data]
  "Takes a sequence of numbers and returns the mean. Sample uses are (mean 1 2 3 4) and (apply mean [1 2 3 4])."
  (let [cnt (count data)]
    (if (> cnt 0)
      (with-precision 10 (/ (apply + data) cnt))
      nil
      )
    )
  )

(defn median [& data]
  "Takes a sequence of numbers and returns the median. Smaple uses are (median 1 2 3 4 5) and (apply median [1 2 3 4 5])"
  (let [cnt (count data)]
    (if (> cnt 0)
      (let [sorted-data (sort data)]
        (cond
          (= cnt 1) (first data)
          :else (if (odd? cnt)
                  (nth sorted-data (/ (dec cnt) 2))
                  (mean (nth sorted-data (/ cnt 2)) (nth sorted-data (dec (/ cnt 2))))
                  ))))
    )
  )

(defn percentile [pct & data]
  (let [total-records (count data)
        sorted-data (sort data)
        adj-pct (cond (<= pct 0) 0.01 
                      (and (>= pct 1) (<= pct 100)) (/ pct 100)
                      (> pct 100) 1
                      :else pct
                )
       ]
       (cond
        (= total-records 0) nil
        (= total-records 1) (first data)
        :else (nth sorted-data (dec (int (Math/ceil (* total-records adj-pct)))))
         )
       )
  )
