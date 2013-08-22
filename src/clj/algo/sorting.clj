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
;; good source of inspiration : [Alexei Sholik's merge sort](https://gist.github.com/alco/2135276)
(defn merge-seqs
  ([[a b]] (merge-seqs a b))
  ([a b]
     (loop [res [] a a b b]
       (let [x (first a) y (first b)]
         (cond
         (every? empty? [a b]) res
         (nil? x) (recur (concat res b) a [])
         (nil? y) (recur (concat res a) [] b)        
         (< x y) (recur (conj res x) (rest a) b)
         :else (recur (conj res y) a (rest b)))))))

(defn merge-sort
  ([coll]
     (if (< (count coll) 2)
       coll        
       (let [q (bit-shift-right (count coll)  1)]
         (merge-seqs (map merge-sort (split-at q coll))))))
  ([p coll]
     (concat (take p coll)
             (merge-sort (drop p coll))))
  ([p r coll] 
     (concat (take p coll)
             (merge-sort (subvec coll p r))
             (drop r coll))))

;; ## Quick Sort
;;
;; It's classical example of recursion, here on JVM need to do tail-call recursion
;;
;; Helpful reading "Joy Of Clojure" by Fogus section 7.3 Thinking recursively
;; GENERALIZED TAIL-CALL OPTIMIZATION
;;

(defn pivot-partition
  "Returns 3 groups partitioning coll at k-th element (the pivot) :
   - First one contains any elements lower than k-th element of coll
   - Second one contains the pivot
   - Third one contains any elements greater than k-th element of coll"
  [k coll]
  (let [pivot (nth coll k)]
    (reduce (fn [[a p b] e]
                        (cond (and (= e pivot) (empty? p))
                              [a [pivot] b]
                              (<= e pivot)
                              [(conj a e) p b]
                              :else
                              [a p (conj b e)]))
             [[] [] []] coll)))

(defn single? [coll]
  (< (count coll) 2))

(defn quick-sort
  ([coll]
     (loop [s [coll]]
       (let [ns (filter seq
                        (mapcat (fn [c] (pivot-partition (dec (count c)) c)) s))
             done (every? single? ns)]
         (if done 
           (map first ns)
           (recur ns)))))
  ([n coll]
     (concat (take n coll) (quick-sort (drop n coll)))))

;; ### Here some other solutions
;;
;; Another solution available [here](http://groups.google.com/group/clojure/browse_thread/thread/6483c6750a4a24c2)
;;
;;    - QA Does lazy-cat can replace by concat ?
;;    - QA Why qsort is not a mundane recursion ?
(defn qsort
  "Quick sort using where pivot is the first element of coll"
  [[pivot & xs]]
  (when pivot
    (let [smaller #(< % pivot)]
      (lazy-cat (qsort (filter smaller xs))
                [pivot]
                (qsort (remove smaller xs))))))

(defn qsort-last
  "Quick sort using where pivot is last element of coll"
  [coll]
  (when-let [pivot (last coll)] 
    (let [smaller #(< % pivot)
          coll (butlast coll)]
      (concat (qsort-last (filter smaller coll))
                [pivot]
                (qsort-last (remove smaller coll))))))

(defn qsort-shuffle [l]
  (letfn [(qsort [[pivot & xs]]
            (when pivot
              (let [smaller #(< % pivot)]
                (lazy-cat (qsort (filter smaller xs))
                          [pivot]
                          (qsort (remove smaller xs))))))]
    (qsort (shuffle l))))

;; ### Some incorrect solutions
;;
;; The most direct translation is raising StackOverflowError
;;
(defn quick-sort-KO
  "Recursive sort algorithm that goes as follows :

   - partition coll with pivot as last element of coll
   - quick sort the 2 sub lists of the partition"
  [coll]
     (if (single? coll)
       coll
       (let [[a p b] (pivot-partition (dec (count coll)) coll)]
         ;;using lazy-cat instead concat gives same error
         (concat (quick-sort-KO a) [p] (quick-sort-KO b)))))

;;
;; QA is quick-sort a case of mutual recursion ? if yes try to use trampoline 
;; 
(defn quick-sort-trampoline-KO [coll]
  (letfn [(quick-sorter [a]
            (fn [] (if (single? a)
                     a
                     (let [[aa p ab] (pivot-partition (dec (count a)) a)]
                       (concat ((quick-sorter) aa) [p] ((quick-sorter) ab))))))
          #_(quick-sorter1 [coll]
            #(if (single? coll)
              coll
              (let [[a b] (pivot-partition (dec (count coll)) coll)]
                (concat (quick-sort- a) (quick-sort- b)))))] 
    (trampoline quick-sorter coll)))

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
