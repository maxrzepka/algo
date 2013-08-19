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

;; http://rosalind.info/problems/fibd/
;;  The total number of pairs of rabbits that will remain after the n-th month (n≤100) if all rabbits live for m months (m≤20).
(defn fibd [n m]
  (reduce
   +
   (nth
    (iterate (fn [gene] (cons (reduce + (next gene)) ;;offspring
                              (butlast gene) ;;skip death ones
                              ))
             (take m (cons 1 (repeat 0))))
    (dec n))))

(defn hamm [s1 s2]
  (+ (count (filter boolean (map (comp not =) s1 s2)))
     (apply - (sort > (map count [s1 s2])))))

(defn mendel [k n m]
  (let [t (+ k n m)
        next-t (map (fn [a b] (map + a b))
                    [[-1 0 0] [0 -1 0] [0 0 -1]]
                    (repeat 3 [k n m]))
        probs [1 1 1 1 0 0.5 1 0.5 0.75]]
    (map *
         probs
         (mapcat (fn [nt pp] (map #(* pp  ( / % (dec t))) nt))
             next-t (map #(/ % t) [k n m])))))

(defn mendel1 [k n m]
  (/ (+ (* k (dec k)) (* 2 k (+ n m))
        (* 0.5 2 n m) (* 0.75 m (dec m)))
   (* (+ n k m) (+ n k m -1))))


(def prot-map
  {"UUU" \F      "CUU" "L"      "AUU" "I"      "GUU" "V"
     "UUC" \F      "CUC" "L"      "AUC" "I"      "GUC" "V"
     "UUA" \L      "CUA" "L"      "AUA" "I"      "GUA" "V"
     "UUG" \L      "CUG" "L"      "AUG" "M"      "GUG" "V"
     "UCU" \S      "CCU" "P"      "ACU" "T"      "GCU" "A"
     "UCC" \S      "CCC" "P"      "ACC" "T"      "GCC" "A"
     "UCA" \S      "CCA" "P"      "ACA" "T"      "GCA" "A"
     "UCG" \S      "CCG" "P"      "ACG" "T"      "GCG" "A"
     "UAU" \Y      "CAU" "H"      "AAU" "N"      "GAU" "D"
     "UAC" \Y      "CAC" "H"      "AAC" "N"      "GAC" "D"
     "UAA" nil     "CAA" "Q"      "AAA" "K"      "GAA" "E"
     "UAG" nil     "CAG" "Q"      "AAG" "K"      "GAG" "E"
     "UGU" \C      "CGU" "R"      "AGU" "S"      "GGU" "G"
     "UGC" \C      "CGC" "R"      "AGC" "S"      "GGC" "G"
     "UGA" nil     "CGA" "R"      "AGA" "R"      "GGA" "G"
     "UGG" \W      "CGG" "R"      "AGG" "R"      "GGG" "G"
     })

(defn prot [s]
  (apply str
   (map
    prot-map
    (map #(apply str %)
          (partition 3 s)))))

(defn parse-fasta [s]
  (->> (clojure.string/split-lines s)
       (partition-by #(if (.startsWith % ">") 1 0))
       (partition 2)
       (map (fn [[[t] s]] (vector (.substring t 1) (apply str s))))
       (into {}))
  )

(defn gc [s]
  (double
   (/ (count (filter #{\G \C} s))
      (count s))))

(defn best-gc [s]
  (sort-by second (map (fn [[a b]] [a (gc b)]) (parse-fasta s))))

;; find positions  where h is detected in s
(defn subs [s h]
  (when (>= (count s) (count h))
    (keep-indexed (fn [i s] (when (= (seq h) s) (inc i)))
     (partition (count h) 1 s))))

;; http://rosalind.info/problems/cons/
;; count occurence of letters at each positions
(defn consensus-count [s]
  (map frequencies (apply map vector (vals (parse-fasta s))))
)

;; (consensus-table (slurp "/home/max/tmp/t2.txt") [\A \C \G \T])
(defn consensus-table [s chars]
  (let [f (consensus-count s)]
    (doseq [c chars]
      (println c ": "
             (clojure.string/join " " (map #(get % c 0) f))))))

(defn consensus [s]
  (apply str
         (map #(->> % (sort-by second) last first) (consensus-count s))))
