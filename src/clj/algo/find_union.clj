(ns algo.find-union)

;;
;; Dynamic connectivity : Union-find data-structure
;; Based on http://algs4.cs.princeton.edu/15uf/
;;


(defprotocol FindUnion
  (connected? [this])
  (fu-find [this element])
  (fu-union [this s1 s2]))

;; elements stores the group for each element
;; groups gives the elements of each group
(defrecord SimpleFindUnion
    [elements groups]
  FindUnion
  (connected? [this] (= 1 (count (:groups this))))
  (fu-find [this element] ((:elements this) element))
  (fu-union [this g1 g2]
    (let [elts1 (set ((:groups this) g1))
          ;; move elements of g1 to g2
          groups (merge-partition (:groups this) g1 g2)
          ;; update group for each element in s1
          elements (into {} (for [[k v] (:elements this)] [k (if (elts1 k) g2 v)]))]
      (SimpleFindUnion. elements groups))))

(defn find-union [vertices]
  (SimpleFindUnion. (zipmap vertices vertices)
                    (zipmap vertices (map vector vertices))))
