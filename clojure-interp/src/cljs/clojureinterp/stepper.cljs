(ns clojureinterp.stepper
  (:require [clojureinterp.util :as U]
            [clojureinterp.eval :as E]))

;; init-state :: AST -> State
(defn init-state [ast]
  (let [decls (:decls ast)
        {s :state} (forM decls compileDecl {})]
    s))

(defn ^:export compile [ast]
  (let [ state      (init-state ast)
         main_addr  (lookup-fn-addr "main" state) ]
   { :control main_addr
     :stack (:stack state)
     :heap  (:heap state)
     :next  (:next state)
     :ret   nil
     :dump  []
     :kont  (fn [ui] (.html ui "Main on the stack")) }))

(defn compileDecl [node state]
  (let [node_name (:node node)]
    (cond
      (= "CFunDef" node_name) 
           (let [fun_name (U.unquotify (:name (:fun_def node)))]
            {:val nil :state (U.addToEnv fun_name node state)})
      (= "CDecl" node_name)
          (let [tipe (U.evalType (:specifiers node))
                {newstate :state} (forM (partial U.evalDecl tipe) (:declarations node) state)]
            {:val nil :state newstate}))))
