(ns algo.planning)

;;
;;
(def b '({:DUREE 9, :PRIX 8, :VOL "LEGACY01", :DEPART 5}
 {:DUREE 9, :PRIX 7, :VOL "YAGNI17", :DEPART 5}
 {:DUREE 7, :PRIX 14, :VOL "META18", :DEPART 3}
 {:DUREE 5, :PRIX 10, :VOL "MONAD42", :DEPART 0}))
;; solution => 18 [MONAD42 LEGACY01]

(def c '({:DUREE 9, :PRIX 8, :VOL "LEGACY01", :DEPART 5}
 {:DUREE 9, :PRIX 7, :VOL "YAGNI17", :DEPART 5}
 {:DUREE 7, :PRIX 14, :VOL "META18", :DEPART 3}
 {:DUREE 4, :PRIX 20, :VOL "ALGO1", :DEPART 2}
 {:DUREE 5, :PRIX 10, :VOL "MONAD42", :DEPART 0}))
;; solution => 20 [ALGO1]




;;
;; copy of https://gist.github.com/cgrand/4557332
;;
(defn top-plans-good
  "First one is the best plan
"
  [bids]
  (reductions (fn [plans {:keys [DEPART VOL DUREE PRIX]}]
                (assoc plans DEPART
                       (max-key #(:gain % 0)
                                (first (vals plans))
                                (let [[[_ {:keys [gain path] :or {gain 0}}]]
                                      (subseq plans >= (+ DEPART DUREE))]
                                  {:gain (+ gain PRIX) :path (cons VOL path)}))))
              (sorted-map) (sort-by :DEPART > bids)))

;; same algo but ordering bids in asc order
(defn top-plans
  "Last one is the best plan
"
  [bids]
  (reductions
   (fn [s {:keys [DUREE PRIX VOL DEPART]}]
     (assoc s (+ DEPART DUREE)
            (max-key #(:total % 0)
                     (last (vals s))
                     (let [{:keys [total path] :or {total 0 path []}}
                           (last (last (subseq s <= DEPART)))]
                       {:total (+ total PRIX) :path (conj path VOL)}))))
   (sorted-map-by <= ) ;; maps arrival => best plan
   (sort-by :DEPART bids)))