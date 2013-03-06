(ns algo.data)

;;
;; Dynamic connectivity : Union-find data-structure
;; Based on http://algs4.cs.princeton.edu/15uf/
;;

(defn merge-inv
  "Return a new vector where cells with value inv2 are updated to inv1"
  [v inv1 inv2]
  (mapv (fn [e] (if (= e inv2) inv1 e)) v))

(defprotocol FindUnion
  (fu-size [this])
  (fu-connected? [this e1 e2])
  (fu-find [this element])
  (fu-union [this s1 s2]))

(defn connected? [data e1 e2]
  (= (fu-find data e1) (fu-find data e2)))

;; elements is mapping elements with their index,
;; groups is a vector where groups[i] is the invariant of the group of i
(defrecord SimpleFindUnion
    [elements groups]
  FindUnion
  (fu-size [this] (-> this :groups distinct count))
  (fu-connected? [this e1 e2]
    (let [[inv1 inv2] (map (partial fu-find this) [e1 e2])]
         (and (= inv1 inv2) (every? (comp not nil?) [inv1 inv2]))))
  (fu-find [this element] (get (:groups this) ((:elements this) element)))
  (fu-union [this e1 e2]
    (if (fu-connected? this e1 e2)
      this
      (let [[inv1 inv2]
            (mapv (partial fu-find this) [e1 e2])]
        (update-in this [:groups] merge-inv inv1 inv2))
      )))

(defn find-union [elements]
  (let [elements (distinct elements)
        size (count elements)]
    (SimpleFindUnion.
     (zipmap elements (range))
     (vec (range size)))))

(defn raw-fu [size]
  (SimpleFindUnion. (vec (range size)) (vec (range size))))
