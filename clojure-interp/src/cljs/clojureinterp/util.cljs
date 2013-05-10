(ns clojureinterp.util)

;; data State = Control * Stack * Heap * Next Location * Return Value * Instruction Dump * UI Kontinuation

;; forM :: (a -> b * s) -> [a] -> s -> [b] * s
(defn forM
  ([f as state] (recur f as []))
  ([f as state values]
    (if (empty? as)
      {:values values :state state}
      (let [{v :val newstate :state} (f (first as))]
        (recur f (rest as) newstate (conj values v))))))

;; addToEnv :: String -> Node -> State -> State
;; addToEnv takes the name of the variable/func (goes on top of the stack), 
;;                the value to be added to the heap,
;;                and the initial state and returns an updated state (updated next)
(defn addToEnv [vname node state]
  (let [{heap :heap stack :stack nxt :next} state
        newstack (conj (assoc (first stack) vname nxt) (rest stack))
        newheap  (assoc heap nxt node)]
    (assoc state :stack newstack :heap newheap :next (inc nxt))))

(defn unquotify [name]
  (let [fquote (fn [s] (if (= \" (first s)) (rest s) s))
        lquote (fn [s] (if (= \" (last s))  (drop-last 1 s) s))]
    (apply str ((comp fquote lquote) name))))

(defn evalType [specs]
  (reduce 
    (fn [specifier acc] 
      (let [spec (:node (:spec specifier))]
        (conj acc spec)))
    [] specs))

(defn initial-value [decl tipe]
  0)

(defn evalDecl [tipe decl state]
   (let [decl_name (unquotify (:name (:declarator decl)))
         init_val  (initial-value decl tipe)]
     {:val nil :state (addToEnv decl_name init_val state)}))

;; lookup-fn-addr :: String -> State -> Addr
(defn lookup-fn-addr [fun state]
  (let [globals (last (:stack state))]
    (get fun globals)))
