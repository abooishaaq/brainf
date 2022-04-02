(defn brainf [& lines]
  (let [src (apply str lines)
        backward (fn [src-ix depth]
                   (if (> depth 0)
                     (if (< src-ix 0)
                       (throw (Exception. "Out of Bounds"))
                       (condp = (nth src src-ix)
                         \[ (if (= depth 1)
                              src-ix
                              (recur (dec src-ix) (dec depth)))
                         \] (recur (dec src-ix) (inc depth))
                         (recur (dec src-ix) depth)))
                     src-ix))
        forward (fn [src-ix depth]
                  (if (> depth 0)
                    (if (>= src-ix (count src))
                      (throw (Exception. "Out of Bounds"))
                      (condp = (nth src src-ix)
                        \] (if (= depth 1)
                             src-ix
                             (recur (inc src-ix) (dec depth)))
                        \[ (recur (inc src-ix) (inc depth))
                        (recur (inc src-ix) depth)))
                    src-ix))]
    (loop [mem [(byte 0)]
           mem-ix 0
           src-ix 0]
      (condp = (get src src-ix)
        \>  (let [next-mem-ix (inc mem-ix)
                  next-mem (if (= next-mem-ix (count mem)) (conj mem (byte 0)) mem)]
              (recur next-mem next-mem-ix (inc src-ix)))
        \<  (recur mem (dec mem-ix) (inc src-ix))
        \+  (recur (update-in mem [mem-ix] inc) mem-ix (inc src-ix))
        \-  (recur (update-in mem [mem-ix] dec) mem-ix (inc src-ix))
        \.  (do
              (print (char (nth mem mem-ix)))
              (flush)
              (recur mem mem-ix (inc src-ix)))
        \,  (let [ch (.read System/in)]
              (recur (assoc mem mem-ix ch) mem-ix (inc src-ix)))
        \[  (recur mem mem-ix (if
                               (zero? (nth mem mem-ix))
                                (forward (inc src-ix) 1)
                                (inc src-ix)))
        \]  (recur mem mem-ix (if
                               (not (zero? (nth mem mem-ix)))
                                (backward (dec src-ix) 1)
                                (inc src-ix)))
        nil nil
        (recur mem mem-ix (inc src-ix))))))


(if (first *command-line-args*)
  (brainf (slurp (first *command-line-args*)))
  (println "usage: clj -M brainf.clj <filepath>"))
