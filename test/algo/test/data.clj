(ns algo.test.data
  (:use algo.data
        clojure.test))

(defn run-fu
  "Execute a sequence of operation FindUnion and returns last one"
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [[size & lines] (line-seq rdr)
          ->int (fn [s] (Integer. (.trim s)))]
      (reduce (fn [m line] (let [[a b] (map ->int (.split line " "))]
                             (fu-union m a b)))
       (raw-fu (->int size)) lines))))

(defn run-fu-mem
  "Execute a sequence of operation FindUnion and
returns data structures at each steps"
  [file]
  (let [[size & lines]
        (clojure.string/split-lines (slurp file))
        edges (map (fn [l] (map #(Integer. %) (.split l " "))) lines)
        init (raw-fu (Integer. size))]
    (reductions (fn [m [a b]] (fu-union m a b)) init edges)))


(def case1
  {:world [:a :b :c :d :e :f]
   :edges [[:a :c]
           [:d :e]
           [:e :d]
           [:e :a]
           [:b :f]
           [:f :e]]
    :sizes [6 5 4 4 3 2 1]
   })

;;TODO find more declarative way for testing
(deftest test-simpleFindUnion
  (let [world [:a :b :c :d :e :f]
        init (find-union (:world case1))
        states (reductions (fn [m [a b]] (fu-union m a b))
                init
                (:edges case1))
        sizes (map fu-size states)
        connected? (fn [m] (fn [[a b]] (fu-connected? m a b)))
        final (last states)]
    (do
      ;;check sizes
      (is (= (:sizes case1) sizes))
      ;;check all elements are connected at the end
      (is (every? boolean (map (connected? final) (:edges case1))))
      ;;check all elements are disconnected at the beginning
      (is (every? (comp not boolean) (map (connected? init) (:edges case1))))
      (is (= 6 (fu-size init)))
      (is (= 1 (fu-size final)))
      (is (= true (fu-connected? final :c :f))))))
