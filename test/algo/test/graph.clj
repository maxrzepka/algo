(ns algo.test.graph
  (:use [clojure.test]
        [algo.graph]))


(def g1 [[:a :b] [:c :b] [:b :d]])

(deftest split-graph-test
  (is (= [#{:a} [[:b 0]]]  (split-graph {:b #{:a}} [[:a 0] [:b 1]]))))

(deftest adjacents-test
  (is (= {:b #{:a}} (adjacents [[:a :b]])))
  (is (= {:b #{:a :c} :d #{:b}} (adjacents g1))))

(deftest degrees-test
  (is (= {:a 0 :b 1} (degrees [[:a :b]])))
  (is (= {:a 0 :c 0 :b 2 :d 1} (degrees g1))))

(deftest topological-sort-test
  (is (= [:a :b] (topological-sort [[:a :b]])))
  (is (= [:a :c :b :d] (topological-sort g1))))

(deftest topological-sort1-test
  (is (= [:a :b] (topological-sort1 [[:a :b]])))
  (is (= [:a :c :b :d] (topological-sort1 g1))))

(deftest dijkstra-test
  (testing "simple graph"
    (let [sp (djikstra-sp g10)]
      (is (= [6 [:s :v :w :t]] (sp :s :t))))))
