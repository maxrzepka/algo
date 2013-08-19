(ns algo.sorting)

;; clojure version of chapter 3 in Algorithms Unlocked by Thomas H. Cormen (2013 MIT)

(defn linear-search
  "Search x in n first elements of coll and returns index when first match found.
If n is not provided, n = size of coll.
"
  ([x coll] (linear-search x nil coll))
  ([x n coll]
     (first
      (first (drop-while
              (fn [[i a]] (not (= a x)))
              (map vector                 
                   (range 0 (inc (if (nil? n) (count coll) n)))
                   coll))
             ))))

;; TODO add pre-condition
(defn binary-search
  "Same as linear-search but quicker and coll must be sorted."
  ([x n coll] (binary-search x 0 n coll))
  ([x p r coll]
     (loop [x x p p r r coll coll]
       (let [q (+ p (int (Math/floor (/ (- r p) 2))))
             a (get coll q)]
         (cond (= x a) q
               (> x a) (recur x p (dec q) coll)
               (< x a) (recur x (inc q) r coll))))))

;; Implementation of the selection sort algorithm
(defn indexed-min [coll]
  (reduce (fn [[e i] [a k]] (if ( < e a) [e i] [a k]))
         (map vector coll (range))))

(defn swap-step [[s c]]
  (cond
   (empty? c) [s c]    
   (= (count c) 1) [(conj s (first c)) []] 
   :else
   (let [[m i] (indexed-min c)]
     [(conj s m)
      (vec
       (concat (subvec c 0 i)
               (subvec c (min (count c) (inc i)))))])))

(defn selection-sort [n coll]
  (first
   (first
    (filter (fn [[s c]] (empty? c))
            (iterate swap-step [[] (subvec (vec coll) 0 n)])))))
