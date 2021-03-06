(ns clojure-stat.core)

(defn summarize-by-group [data group & stats]
"Generates partitioned summary statistics for a collection of maps. Sample proceeds:
   (def sample_data [
          {:dt 20110331 :name \"John\" :age 22 :salary 65000 :education 12}
          {:dt 20120331 :name \"John\" :age 23 :salary 65000 :education 12}
          {:dt 20110331 :name \"Jane\" :age 22 :salary 100000 :education 16}
          {:dt 20120331 :name \"Jane\" :age 23 :salary 101000 :education 16}
          {:dt 20110331 :name \"Eric\" :age 22 :salary 25000 :education 12}
          {:dt 20120331 :name \"Eric\" :age 23 :salary 25000 :education 12}
          ])
  (summarize-by-group sample_data [:education :age] [:salary + \"aggregateIincome\"])
  (summarize-by-group sample_data [:age] [:salary + \"aggregateIncome\"] [:education mean \"avgEducation\"])
  (summarize-by-group sample_data :age [:salary + \"aggregateIncome\"] [:education mean \"avgEducation\"])
  (summarize sample_data [:salary + \"aggregate_income\"])
  (summarize sample_data [:salary + \"aggregate_income\"] [:education mean \"avgEducation\"])
"
  (let [group (if (vector? group) group [group])
        sorted-data (sort-by (apply juxt group) data)
        partitioned-data (partition-by 
                           (fn [data] (select-keys data group)) 
                           sorted-data)]
    (map
      (fn [partition]
        (let [grouping-data (reduce 
                              (fn [result obs] 
                                (assoc result obs 
                                       (obs (first partition)))) 
                              {} 
                              group)
              summarize-fn (partial summarize partition)
              statistics (apply summarize-fn stats)              
              ]
          (merge grouping-data statistics)
          )
        )
      partitioned-data
      )    
    )
  )

(defn summarize [data & stats]
    (let [reducer-fn (fn [result [keywd stat-fn name]] (conj result (keyword name) (apply stat-fn (map keywd data))))
          stat-results (reduce reducer-fn [] stats)]
      (apply hash-map stat-results)        
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
