(ns looping-is-recursion)

(defn power [base exp]
  (loop [i 0
         res 1]
    (if (= i exp)
      res
    (recur (inc i) (* res base)))))

(defn last-element [a-seq]
  (if (nil? (next a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq)
          (empty? b-seq))
       true
     (and (seq a-seq)
          (seq b-seq)
          (= (first a-seq)
             (first b-seq)))
       (seq= (rest a-seq)
             (rest b-seq))
     :else
       false
    ))

(defn find-first-index [pred a-seq]
  (loop [ind 0
         the-seq a-seq]
    (if (empty? the-seq)
      nil
      (if (pred (first the-seq))
        ind
        (recur (inc ind) (rest the-seq))))))

(defn avg [a-seq]
  (loop [acc 0
         cnt 0
         the-seq a-seq]
    (if (empty? the-seq)
      (/ acc cnt)
      (recur (+ acc (first the-seq)) (inc cnt) (rest the-seq)))))

(defn parity [a-seq]
  (reduce
   (fn [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))
   #{} a-seq))


(defn fast-fibo [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b (+ a b) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [sofar #{}
         res []
         the-seq a-seq]
    (if (empty? the-seq)
      res
      (if (contains? sofar (first the-seq))
        res
        (recur (conj sofar (first the-seq))
               (conj res (first the-seq))
               (rest the-seq))))))
