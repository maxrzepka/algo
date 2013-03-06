(ns algo.test.graph
  (:use [clojure.test]
        [algo.graph]))

(deftest test-dijkstra
  (testing "simple graph"
    (let [sp (djikstra-sp g10)]
      (is (= [6 [:s :v :w :t]] (sp :s :t))))))
