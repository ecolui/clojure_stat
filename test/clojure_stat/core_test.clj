(ns clojure-stat.core-test
  (:require [clojure.test :refer :all]
            [clojure-stat.core :refer :all]))

(deftest mean-test
  (testing "mean Function"
    (is (= 10 (mean 8 9 10 11 12)) "testing multiple arguments")    
    (is (= 10 (mean 10)) "testing 1 arg")        
    (is (= nil (mean)) "testing 0 args")        
    (is (= -10 (mean -8 -9 -10 -11 -12)) "testing negative numbers")        
    (is (= 0 (mean 0)) "testing zero using 1 arg")        
    (is (= 0 (mean -1 1)) "testing zero using 2 args")        
    )
  )

(deftest median-test
  (testing "median Function"
    (is (= 7 (median 5 4 9 10 0 -5 100 200)) "Even Number of arguments - take avg of 5 and 9")
    (is (= 5 (median 5 4 9 10 0 -5 100)) "Odd Number of arguments")
    (is (= 5 (median 5)) "One arg")
    (is (= nil (median)) "No args")
    (is (= -5 (median -5 -9 -3 -2 -10)) "Negative args, even number of args")
    (is (= -7 (median -5 -9 -3 -2 -10 -100)) "Negative args, odd number of args")
    )
  )

(deftest percentile-test
  (testing "percentile function"
    (is (= 1 (percentile 10 5 4 3 2 1 10 9 8 7 6)) "test whole number 10 percentile")
    (is (= 1 (percentile 0.1 5 4 3 2 1 10 9 8 7 6)) "test decimal 0.1 percentile")
    (is (= nil (percentile 10)) "no numbers in sequence")
    (is (= 55 (percentile 10 55)) "only 1 number in sequence")
    (is (= -25 (percentile 10 -5 -4 -3 -2 -1 -25 -9 -8 -7 -6)) "test negative numbers 10 percentile")
    (is (= -9 (percentile 20 -5 -4 -3 -2 -1 -25 -9 -8 -7 -6)) "test negative numbers 10 percentile")
    )
  )

(deftest summarize-test
  (let [sample_data [
          {:dt 20110331 :name "John" :age 22 :salary 65000 :education 12}
          {:dt 20120331 :name "John" :age 23 :salary 65000 :education 12}
          {:dt 20110331 :name "Jane" :age 22 :salary 100000 :education 16}
          {:dt 20120331 :name "Jane" :age 23 :salary 101000 :education 16}
          {:dt 20110331 :name "Eric" :age 22 :salary 25000 :education 12}
          {:dt 20120331 :name "Eric" :age 23 :salary 25000 :education 13}
          ]
        ]

    (testing "2 statistics"
      (let [result (summarize sample_data [:dt :education] [[:salary mean "avg_salary"] [:salary + "aggregate_income"]])]
        (is (= (mean 65000 25000) (:avg_salary (first result))))
        (is (= (+ 65000 25000) (:aggregate_income (first result))))      
        (is (= (count result) 5) )
        )
    )

    (testing "1 statistic only. Statistic is in a nested vector."
      (let [result (summarize sample_data [:education :age] [[:salary + "aggregate_income"]])]
        (is (= (+ 65000 25000) (:aggregate_income (first result))) )
        (is (= (count result) 5) )
        )
    )

    (testing "1 statistic only. Statistic is NOT in a nested vector."
      (let [result (summarize sample_data [:education :age] [:salary + "aggregate_income"])]
        (is (= (+ 65000 25000) (:aggregate_income (first result))) )
        (is (= (count result) 5) )
        )
    )

    (testing "1 statistic only but no group-by arguments"
      (let [
        result (summarize sample_data [:salary + "aggregate_income"])
        agg_inc (reduce (fn [x y] (+ x y)) (map :salary sample_data))
        ]
        (is (= agg_inc (:aggregate_income result)) 
          "Summarize returns a simple map, not a vector with one element, which is why we don't call (first result)
           like the other tests")
        )
    )

    )
  )



