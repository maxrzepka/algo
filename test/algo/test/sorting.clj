(ns algo.test.sorting
  (:use algo.sorting
        clojure.test))

(def datasets
  "set of unsorted list and its sorted one "
  { [4 2 1 10] [1 2 4 10]
    [2 3 4 5 -1] [-1 2 3 4 5]
    [1 2 4 1] [1 1 2 4]
    [1 2 4 7] [1 2 4 7]
    [89 1 2 3 4 5] [1 2 3 4 5 89]
    })

(deftest linear-search-test
  (is (= 3 (linear-search 8 [6 9 6 8])))
  (is (= 2 (linear-search 9 4 [2 2 9 3 4 5 10]))))

(deftest middle-test1
  (doseq [n (range 1 20)]
    (is (= (bit-shift-right n 1) (middle 0 n)))))

(deftest middle-test
  (is (= 1 (middle 0 2) ))
  (is (= 2 (middle 1 3) ))
  (is (= 7 (middle 4 10)))
  (is (= 3 (middle 0 6) ))
  (is (= 3 (middle 0 7) )))

(deftest binary-search-test
  (is (= 3 (linear-search 8 4 [6 9 6 8])))
  (is (= 2 (linear-search 9 4 [2 2 9 3 4 5 10]))))

(deftest indexed-min-test
  (is (= [1 3] (indexed-min [2 3 4 1])))
  (is (= [2 2] (indexed-min [4 9 2 2]))))

(deftest swap-step-test
  (is (= [[-5] [2 3 4]] (swap-step [[] [2 3 4 -5]]))))

(deftest selection-sort-test
  (is (= [1 2 3 4] (selection-sort 4 [2 3 4 1 7])))
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (selection-sort unsorted))))
  )

(deftest insert-by-order-test
  (is (= [1 2 3 4] (insert-by-order 3 [1 2 4])))
  (is (= [1 2 4 10] (insert-by-order 10 [1 2 4])))
  (is (= [1 1 4 10] (insert-by-order 1 [1 4 10])))
  (is (= [1 4 4 4 10] (insert-by-order 4 [1 4 4 10]))))

(deftest insertion-sort-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (insertion-sort unsorted)))))

(deftest insertion-sort1-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (insertion-sort1 unsorted)))))

(deftest merge-seqs-test
  (is (= [1 2 3 4 7 10] (merge-seqs [1 4 7] [2 3 10]))))

(deftest merge-sort-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (merge-sort unsorted)))))

(deftest merge-sub-sort-test
  (is (= [9 1 2 3 10] (merge-sort 1 [9 10 1 3 2])))
  (is (= [9 1 10 2 3] (merge-sort 1 3 [9 10 1 2 3]))))

(deftest pivot-partition-test
  (is (= [[4 2] [5] [10 89 30]] (pivot-partition 2 [10 4 5 89 2 30])))
  (is (= [[4 -89 2 5] [5] [10 30]] (pivot-partition 2 [10 4 5 -89 2 5 30]))))

(deftest quick-sort-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (quick-sort unsorted)))))

(deftest qsort-last-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (qsort-last unsorted)))))

#_(deftest counting-sort-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (counting-sort unsorted)))))

#_(deftest radix-sort-test
  (doseq [[unsorted sorted] (seq datasets)]
    (is (= sorted (radix-sort unsorted)))))

;; TODO Test performance 
