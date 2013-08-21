(ns algo.sorting)
;; # Sorting Unlocked
;;
;; Clojure version of chapter 3-4 from [Algorithms Unlocked]() by [Thomas H. Cormen](http://twitter.com/thcormen) (2013 MIT)

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

(defn middle
  "Returns the midpoint between p and r"
  [p r]
  (+ p (int (Math/floor (/ (- r p) 2)))))

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

;; ## Selection Sort
;;
(defn indexed-min [coll]
  (reduce (fn [[e i] [a k]] (if (> e a) [a k] [e i]))
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

(defn selection-sort
  ([coll] (selection-sort (count coll) coll))
  ([n coll]
     (first
      (first
       (filter (fn [[s c]] (empty? c))
               (iterate swap-step [[] (subvec (vec coll) 0 n)]))))))

;; ## Insertion Sorting
;;
(defn in-between? [x a b]
  (cond (nil? a) (<= x b)
        (nil? b) (<= a x)
        :else (<= a x b)))

(defn insert-by-order
  "returns scoll with x inserted at the right place."
  [x scoll]
  (if (or (empty? scoll) (<= (last scoll) x))
    ;; case when scoll is empty or x 
    (conj (vec scoll) x)
    (second
     (reduce (fn [[t a] e]
               (if t
                 (let [insertion? (in-between? x (last a) e)
                       a (if insertion? (conj a x) a)]
                   [(not insertion?) (conj a e)])
                 [t (conj a e)]))
             [true []]  scoll))))

(defn insertion-sort
  ([coll] (loop [scoll [] coll coll]
            (if (empty? coll)
              scoll
              (recur (insert-by-order (first coll) scoll) (rest coll)))))
  ([n coll]
     (concat (insertion-sort (take n coll)) (drop n coll))))

(defn insertion-sort1
  ([coll] (reduce (fn [a e] (insert-by-order e a)) 
                  [] coll))  
  ([n coll]
     (concat (insertion-sort1 (take n coll)) (drop n coll))))

;; ## Merge Sort
;;
;; inspired (partially copied) from [Alexei Sholik's gist](https://gist.github.com/alco/2135276)
(defn merge-seqs
  ([[a b]] (merge-seqs a b))
  ([a b]
     (loop [res [] a a b b]
       (cond
        (every? empty? [a b]) res
        (empty? a) (recur (concat res b) a [])
        (empty? b) (recur (concat res a) [] b)        
        (< (first a) (first b)) (recur (conj res (first a)) (rest a) b)
        :else (recur (conj res (first b)) a (rest b))))))

(defn merge-sort
  ([coll]
     (cond (< (count coll) 2) coll
           :else
           (let [q (bit-shift-right (count coll)  1)]
             (merge-seqs (map merge-sort (split-at q coll))))))
  ([p r coll]
     (concat (take p coll)
             (merge-sort (subvec coll p r))
             (drop r coll))))

;; ## Quick Sort

(defn quick-sort
  ([coll] (quick-sort (count coll) coll))  
  ([n coll]
     coll))

;; ## Counting Sort

(defn counting-sort
  ([coll] (counting-sort (count coll) coll))  
  ([n coll]
     coll))

;; ## Radix Sort

(defn radix-sort
  ([coll] (radix-sort (count coll) coll))  
  ([n coll]
     coll))
