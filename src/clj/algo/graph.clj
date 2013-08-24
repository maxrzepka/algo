(ns algo.graph
  (:require [algo.data :as d]))

;;
;; ## Directed Acyclic Graph
;;
;; Directed graph are represented as a list of t-uple (u,v) where u -> v
;;
;; 
(def goalie
  "Example taken from Chap 5 of Algo Unlocked by T.H. Cormen"
  [[:undershorts :compresion-shorts]
             [:compresion-shorts :hose]
             [:compresion-shorts :cup]
             [:socks :hose]
             [:hose :pants]
             [:cup :pants]
             [:pants :skates]
             [:skates :leg-pads]
             [:t-shirt :chest-pad]
             [:pants :sweater]
             [:chest-pad :sweater]
             [:sweater :mask]
             [:mask :catch-glove]
             [:leg-pads :catch-glove]
             [:catch-glove :blocker]])


(defn split-graph
  "Given a map of vertices to their set of parents,
returns (set of top vertices, list of degrees of graph without top vertices)"
  [m degrees]
  (let [top (set (keep (fn [[u d]] (when-not (> d 0) u)) degrees))]
    [top (keep (fn [[u d]]
            (when-not (top u) 
              [u (- d (count (m u #{})))]))
          degrees)]))

(defn adjacents
  "Given a directed graph,
returns a map of vertices to their parent vertices"
  [g]
  (reduce (fn [a [u v]] (update-in a [v] (fnil conj #{}) u)) {} g))

(defn degrees
  "Given a directed graph,
returns a map of all vertices with their degree (ie # of parent vertices)"
  [g]
  (let [vertices (set (mapcat identity g))]
    (reduce (fn [a [u v]] (update-in a [v] inc))
            (into {} (map vector vertices (repeat 0)))
            g)))

(defn topological-sort
  "Returns a list of vertices in linear order
g is a list of 2-uple of vertices (or nodes) (ie a directed graph) "
  [g]
  (let [m (adjacents g)
        deg (seq (degrees g))]
    ;;TODO is loop equivalent to iterate ?
    (loop [deg deg res []]
      (if (seq deg)
        (let [[top new-deg] (split-graph m deg)]
          (recur new-deg (concat res top)))
        res))))

;; loop is equivalent to iterate.
;; 

(defn topological-stepper [g]
  (let [m (adjacents g)]
    (fn [[deg res]]
      (if (empty? deg)
        [deg res]
        (let [[top new-deg] (split-graph m deg)]
          [new-deg (concat res top)])))))

(defn topological-sort1
  "Same as topological-sort but using iterate instead of loop"
  [g]
  (->> (iterate (topological-stepper g) [(degrees g) []])
       (drop-while (comp not empty? first))
       first
       second))

;; ## Greedy Algorithms
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
;; mst of g2 = #{:a :b} #{:a :c} #{:b :d}

;;
;; Prism's Maximum spanning Tree
;;
(defn prism-mst
  "Find the edges connecting all vertices of the graph with minimun cost :
  1. Initialiaze visited set with any vertices
  2. Append the vertice not already visited with the minimal cost
  3. Stop when all vertices are visited
"
  [g]
  (let [vertices (set (apply concat (keys g)))
        start (first vertices)]
    (loop [x #{start} t {}]
          (if (= x vertices) ;;stop when all vertices visited
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

;;
;; Kruskal's Maximum Spanning Tree
;;

(defn kruskal-mst
  "Find the edges connecting all vertices of the graph with minimun cost :
   1. Order the edges by cost
   2. Loop through the ordered edges
   3. include an edge when it doesn't introduce any cycle
   4. Stop when all vertices are connected
"
  [g]
  (let [edges (map first (sort-by second g))
        vertices (reduce (fn [s e] (into s e)) (keys g))]
    (loop [t []
           sfu (d/find-union vertices)
           r edges]
      (cond
       (= 1 (d/fu-size sfu)) ;;stop when all vertices are connected
       t
       (nil? (seq r)) ;; graph not connected
       nil
       :else (let [[s1 s2] (map #(d/fu-find sfu %) (first r))]
               (if (= s1 s2)
                 ;;cycle detected : skip this edge
                 (recur t sfu (next r))
                 ;;edge accepted
                 (recur (conj t (first r))
                        (d/fu-union sfu s1 s2)
                        (next r))))))))
