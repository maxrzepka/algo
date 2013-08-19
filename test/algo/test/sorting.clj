(ns algo.test.sorting
  (:use algo.sorting
        clojure.test))

(deftest linear-search-test
  (is (= 3 (linear-search 8 [6 9 6 8]))
      (= 2 (linear-search 9 4 [2 2 9 3 4 5 10]))))

(deftest binary-search-test
  (is (= 3 (linear-search 8 4 [6 9 6 8]))
      (= 2 (linear-search 9 4 [2 2 9 3 4 5 10]))))

(deftest indexed-min-test
  (is (= [1 3] (indexed-min [2 3 4 1]))
      (= [2 2] (indexed-min [4 9 2 2]))))

(deftest swap-step-test
  (is (= [[-5] [2 3 4]] (swap-step [[] [2 3 4 -5]]))))

(deftest selection-sort-test
  (is (= [1 2 3 4] (selection-sort 4 [2 3 4 1 7]))))
