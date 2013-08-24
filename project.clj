(defproject algo "0.1.0-SNAPSHOT"
  :description "Well known Algorithms implemented in clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium "0.3.1"]]
  :source-paths       ["src" "src/clj"]
  ;:java-source-paths  ["src/java"]
  :test-selectors {:default #(not (some #{:benchmark} (keys %)))
                   :benchmark :benchmark}
  )
