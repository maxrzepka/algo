(ns algo.graph)

;; Greedy Algorithms
;;
;; Djikstra's Shortest Path
;;


;; # Graph representation
;;
;;  Undirected graph is a map : set of 2 vertices => edge's weight
;;
(def g10 {#{:s :v} 1 #{:v :w} 2 #{:s :w} 4 #{:w :t} 3 #{:v :t} 6})
;; shortest path (:s -> :t) is :s :v :w :t with cost 6

(def g11 {#{:a :b} 2 #{:a :c} 3 #{:b :e} 1 #{:e :c} 1})
;; shortest path ( :a -> :c ) is :a :c

(defn djikstra-sp
  "Returns a fct computing the shortest path between 2 vertices.
"
  [g]
  (let [edges (set (apply concat (keys g)))]
    (fn sp
      [s e]
      (when (and (edges e) (edges s)) ;; vertices should exist in g
        (loop [x {s 0} paths {s [s]}]
          (if (x e)
            ;;[x paths]
            (map #(get % e) [x paths])
            (let [vs (into (sorted-map-by <=)
                           ;;get all edges where one vertice is in x and the other not
                           (for [[v c] g
                                 x1 v
                                 y1 v
                                 :let [a (x x1)]
                                 :when (and (x x1)
                                            ((comp not x) y1))]
                             [(+ a c) [x1 y1]]))
                  ;;vs is list of edges from x to outside x ordered by cost
                  ;;edge satisfying greedy constraint
                  [c1 [x1 y1]] (first vs)
                  ;;z (println "add path from " x1 " to " y1)
                  p1 (conj (paths x1) y1)
                  ]
              (recur (assoc x y1 c1) (assoc paths y1 p1)))))))))


(def g2 {#{:a :b} 1 #{:b :c} 3 #{:a :c} 2 #{:b :d} 4 #{:d :c} 5})

;;
;; Prism's Maximum spanning Tree
;;
(defn prism-mst
  "Find the edges connecting all vertices of the graph with minimun cost :
Start from any vertices
"
  [g]
  (let [edges (set (apply concat (keys g)))
        start (first edges)]
    (loop [x #{start} t {}]
          (if (= x edges)
            t
            (let [vs (into (sorted-map-by <=)
                           ;;get all edges where one vertice is in x and the other not
                           (for [[s c] g
                                 :when (and (some x s)
                                            (some (comp not x) s))]
                             [c s]))
                  ;;vs the list of edges from x to outside x ordered by cost
                  ;;edge satisfying greedy constraint
                  [c1 v1] (first vs)
                  ]
              (recur (into x v1) (assoc t v1 c1)))))))