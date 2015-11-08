(ns looping-is-recursion
  (:use clojure.repl))

(defn power
  ;; recursive
  ([base exp]
   (if (zero? exp)
     1
     (* base (power base (dec exp))))))

(defn power
  ;; tail recur with function-helper
  ([base exp]
   (let [helper (fn [acc exp]
                  (if (zero? exp)
                    acc
                    (recur (* acc base) (dec exp))))]
     (helper 1 exp))))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (not (empty? seq2))) false
   (and (not (empty? seq1)) (empty? seq2)) false
   (not= (first seq1) (first seq2)) false
   (and (empty? seq1) (empty? seq2)) true
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         checked-seq a-seq]
    (cond
     (empty? checked-seq) nil
     (pred (first checked-seq)) i
     :else (recur (inc i) (rest checked-seq)))))

(defn avg [a-seq]
  (/ (apply + a-seq) (count a-seq)))

(defn avg [a-seq]
  (/ (reduce + a-seq) (count a-seq)))

(defn avg [a-seq]
  (loop [cnt 0
         sum 0
         checked-seq a-seq]
    (if (empty? checked-seq)
      (/ sum cnt)
      (recur (inc cnt) (+ sum (first checked-seq)) (rest checked-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [parity-set #{}
           checked-seq a-seq]
      (if (empty? checked-seq)
        parity-set
        (recur
         (toggle parity-set (first checked-seq))
         (rest checked-seq))))))

(defn fibo
;;   recursion
  [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fibo (- n 1)) (fibo (- n 2)))))


(defn fast-fibo
  ;; tail recur
  [n]
  (loop [acc 0
         n-1 1
         current-n n]
    (if (zero? current-n)
      acc
      (recur (+ acc n-1) acc (dec current-n)))))


(defn cut-at-repetition [a-seq]
  (loop [acc-seq []
         checked-seq a-seq]
    (if (or (empty? checked-seq) (contains? (set acc-seq) (first checked-seq)))
      acc-seq
      (recur (conj  acc-seq  (first checked-seq)) (rest checked-seq)))))
