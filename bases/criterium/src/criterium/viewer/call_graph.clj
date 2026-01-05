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
