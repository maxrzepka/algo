(ns algo.bioinfo)

;; DNA http://rosalind.info/problems/dna/
;;(b/dna1 "ACGT" "AGCTTTTCATTCTGACTGCAACGGGC")
(defn dna1 [letters sequence]
  (vals
   (reduce (fn [m c]
             (when ((set letters) c)
               (update-in m [c] inc)))
           (apply sorted-map (interleave letters (repeat 0)))
           sequence)))

(defn dna2 [letters sequence]
  (let [getter (fn [c] #(get % c))]
   ((apply juxt (map getter letters)) (frequencies sequence))))

;; MA http://rosalind.info/problems/rna/
;; (ma "GATGGAACTTGACTACGTAAATT" \T \U)
(defn ma [dna & pairs]
  (let [pairs (apply hash-map pairs)]
    (apply str
           (map (fn [c] (pairs c c)) dna))))

;; http://rosalind.info/problems/revc/
;; (revc "AAAACCCGGT" \A \T \C \G)
(defn revc [dna & pairs]
  (let [pairs (apply hash-map
                     (mapcat (fn [p] (concat p (reverse p)))
                             (partition 2 pairs)))]
    (apply str
           (map (fn [c] (pairs c c)) (reverse dna)))))

;; http://rosalind.info/problems/fib/
;; (fib n k)
(defn fib [n k]
  (second
   (nth
    (iterate (fn [[p pp]] [(+ p (* k pp)) p]) [1 1])
    (dec n))))
