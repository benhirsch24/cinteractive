(defproject clojure-interp "0.1.0"
  :description "Interactive C Interpreter - Clojurescript code for CESK machine"
  :url "http://github.com/benhirsch24/cinteractive"
  :source-paths ["src/clj"]
  :plugins [[lein-cljsbuild "0.3.0"]
            [org.clojure/core.match "0.2.0-alpha12"]]
  :cljsbuild
      {:builds
        [{ :source-paths ["src/cljs"]
           :compiler
            { :pretty-print true
              :output-to "resources/public/cesk.js"
              :optimizations :whitespace }}]})
