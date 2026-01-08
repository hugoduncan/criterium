(ns criterium.viewer.call-graph
  "Text viewer for call tree data from method tracing.

  Renders call trees as ASCII trees with box-drawing characters,
  showing call counts and call count percentages."
  (:require
   [clojure.string :as str]
   [criterium.view :as view]))

;;; Call Count Calculation

(defn total-call-count
  "Calculate the total call count across all nodes in the tree.
  Sums the :call-count of each node recursively."
  ^long [node]
  (if (nil? node)
    0
    (+ (long (or (:call-count node) 0))
       (long (reduce + 0 (map total-call-count (:children node)))))))

;;; ASCII Tree Rendering

(defn- format-node-label
  "Format a single node's label with class.method and call statistics."
  [node ^long total-calls]
  (let [class-name (:class node)
        method (:method node)
        call-count (long (or (:call-count node) 0))
        percentage (if (pos? total-calls)
                     (* 100.0 (/ (double call-count) (double total-calls)))
                     0.0)
        call-word (if (= 1 call-count) "call" "calls")]
    (format "%s.%s (%d %s, %.1f%%)"
            (or class-name "<unknown>")
            (or method "<unknown>")
            call-count
            call-word
            percentage)))

(defn- render-tree-lines
  "Render a call tree node and its children as a sequence of lines.

  prefix - the string prefix for this node's line (box-drawing chars)
  child-prefix - the prefix to use for children's lines
  node - the call tree node to render
  total-calls - total call count for percentage calculation"
  [prefix child-prefix node total-calls]
  (when node
    (let [label (format-node-label node total-calls)
          children (:children node)
          num-children (count children)]
      (cons
       (str prefix label)
       (mapcat
        (fn [idx child]
          (let [last? (= idx (dec num-children))
                branch (if last? "└── " "├── ")
                continuation (if last? "    " "│   ")]
            (render-tree-lines
             (str child-prefix branch)
             (str child-prefix continuation)
             child
             total-calls)))
        (range num-children)
        children)))))

(defn render-call-tree
  "Render a call tree as an ASCII tree string.

  Returns a string with the tree structure using box-drawing characters.
  Each node shows: class.method (N calls, X.X%)"
  [call-tree]
  (when call-tree
    (let [total-calls (total-call-count call-tree)
          lines (render-tree-lines "" "" call-tree total-calls)]
      (str/join "\n" lines))))

(defn print-call-tree
  "Print a call tree to *out* with a header showing total calls."
  [{:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (total-call-count call-tree)]
        (println (format "Call Tree (%d total calls)" total-calls))
        (println (render-call-tree call-tree))))))

;;; View Implementation

(defmethod view/call-tree* :print
  [_ options data-map]
  (print-call-tree options data-map))

(defmethod view/call-flame* :print
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (println "Call Flame Chart (use :portal or :kindly viewer for visual display)"))))

;;; Most-Called View

(defn- format-location
  "Format file:line location, or return nil if not available."
  [file line]
  (when (and file (pos? (or line 0)))
    (str file ":" line)))

(defn- clojure-invoke-method?
  "Check if a method name is a Clojure function invocation method."
  [method]
  (contains? #{"invoke" "invokeStatic" "invokePrim" "doInvoke"} method))

(defn- extract-clojure-fn-name
  "Extract Clojure function name from class name like 'myns.core$my_fn'.
  Returns the part after the last $ converted from underscores to hyphens."
  [class-name]
  (when class-name
    (when-let [idx (str/last-index-of class-name "$")]
      (-> (subs class-name (inc idx))
          (str/replace "_" "-")
          (str/replace "BANG" "!")
          (str/replace "QMARK" "?")
          (str/replace "STAR" "*")
          (str/replace "PLUS" "+")
          (str/replace "GT" ">")
          (str/replace "LT" "<")
          (str/replace "EQ" "=")))))

(defn- format-method-name
  "Format a method name for display.
  For Clojure invoke methods, shows the function name.
  For Java methods, shows class.method."
  [class-name method]
  (if (and (clojure-invoke-method? method)
           (str/includes? (or class-name "") "$"))
    (or (extract-clojure-fn-name class-name)
        (str class-name "." method))
    (str (or class-name "<unknown>") "." (or method "<unknown>"))))

(defn print-most-called
  "Print a table of most frequently called methods."
  [{:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (println (format "\nMost Called Methods (top %d, %d total calls in list)"
                         (count methods) total-in-list))
        (println (str/join "" (repeat 70 "-")))
        (println (format "%-4s %-40s %10s  %s" "Rank" "Method" "Calls" "Location"))
        (println (str/join "" (repeat 70 "-")))
        (doseq [[idx {:keys [class method file line total-calls]}] (map-indexed vector methods)]
          (let [display-name (format-method-name class method)
                location (or (format-location file line) "")]
            (println (format "%-4d %-40s %10d  %s"
                             (inc idx)
                             (if (> (count display-name) 40)
                               (str (subs display-name 0 37) "...")
                               display-name)
                             total-calls
                             location))))
        (println (str/join "" (repeat 70 "-")))))))

(defmethod view/most-called* :print
  [_ options data-map]
  (print-most-called options data-map))
