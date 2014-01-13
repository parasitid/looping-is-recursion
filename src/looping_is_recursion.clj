(ns looping-is-recursion)

(defn power [base exp]
  (let [
        helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))
        ]
    (helper 1 base exp)))



(defn last-element [a-seq]
  (let [helper (fn [prev a-seq] (if (empty? a-seq) prev (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                  (not acc) false
                  (and (empty? seq1) (empty? seq2)) acc
                  (or (empty? seq1) (empty? seq2)) false
                  :else (recur (== (first seq1) (first seq2)) (rest seq1) (rest seq2))))
        ]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ index 0
         lseq a-seq]
    (cond
     (empty? lseq) nil
     (pred (first lseq)) index
     :else (recur (inc index) (rest lseq)))))

(defn avg [a-seq]
  (let [nb (count a-seq)]
    (loop [sum 0 lseq a-seq ] 
    (if (empty? lseq) (/ sum nb) (recur (+ sum (first lseq)) (rest lseq))))))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         lseq a-seq]
    (if (empty? lseq)
      acc
      (recur (toggle acc (first lseq)) (rest lseq)))))


(defn fast-fibo [x]
  (loop [ 
          n2 0
          n1 1
          acc 0
          n 0]
    (cond 
     (== n x) acc
     :else (recur (+ acc n2) acc (+ acc n1)  (inc n)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         lseq a-seq]
    (if (or (empty? lseq) (contains? (set acc) (first lseq)))
      acc
      (recur (conj acc (first lseq)) (rest lseq)))))
