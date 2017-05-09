(ns bfi.core
  (:gen-class))

(defn- read-tokens
  "read string operations and return operation list.
  Not operation character is skipped."
  [op-str]
  (keep #(case %
           \> :>
           \< :<
           \+ :inc
           \- :dec
           \. :dot
           \, :colon
           \[ :lpar
           \] :rpar
           nil)
        op-str))

(defn- read-char
  "read character from standard input."
  []
  (first (read-line)))

(defn- build-structure
  "build parenthesis structure from op-seq."
  [op-seq]
  (letfn [(helper [op-seq s-seq lpar-exist]
            (cond
              (empty? op-seq) (if (not lpar-exist)
                                s-seq
                                (throw (Exception. "right parenthesis not found.")))
              (and lpar-exist (= (first op-seq) :rpar)) [s-seq (rest op-seq)]
              (= (first op-seq) :lpar) (let [[s ss] (helper (rest op-seq) [] true)]
                                         (recur ss (conj s-seq s) lpar-exist))
              :else (recur (rest op-seq) (conj s-seq (first op-seq)) lpar-exist)))]
    (helper op-seq [] false)))

(defn- exec-inst
  "exec one instruction and return memory and memory pointer after execution."
  [inst mem m-ptr]
  (case inst
    :> [mem (inc m-ptr)]
    :< [mem (dec m-ptr)]
    :inc [(update-in mem [m-ptr] inc) m-ptr]
    :dec [(update-in mem [m-ptr] dec) m-ptr]
    :dot (do (print (char (mem m-ptr))) [mem m-ptr])
    :colon (let [c-int (int (read-char))]
             [(assoc mem m-ptr c-int) m-ptr])))

(defn- bfi-helper [s-insts i-ptr mem m-ptr is-inner]
  (cond
    (<= (count s-insts) i-ptr) (if (and is-inner (not (zero? (mem m-ptr))))
                                 (recur s-insts 0 mem m-ptr is-inner)
                                 [mem m-ptr])
    (coll? (s-insts i-ptr)) (if (not (zero? (mem m-ptr)))
                              (let [[mem m-ptr] (bfi-helper (s-insts i-ptr) 0 mem m-ptr true)]
                                (recur s-insts (inc i-ptr) mem m-ptr is-inner))
                              (recur s-insts (inc i-ptr) mem m-ptr is-inner))
    :else (let [[mem m-ptr] (exec-inst (s-insts i-ptr) mem m-ptr)]
            (recur s-insts (inc i-ptr) mem m-ptr is-inner))))

(defn- bfi
  "entry point."
  [op-str]
  (let [insts (read-tokens op-str)
        s-insts (build-structure insts)]
    (bfi-helper s-insts 0 (vec (repeat 30000 0)) 0 false)))

(defn -main [& args]
  (if (zero? (count args))
    (println "usage: java -jar bfi.jar <source file path>")
    (do
      (bfi (slurp (first args)))
      (println))))
