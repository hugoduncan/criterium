(ns criterium.viewer.common.allocation
  "Allocation view helpers for formatting and rendering allocation data.

  Provides functions for formatting call sites and object types, as well as
  ASCII treemap rendering for allocation visualization."
  (:require
   [clojure.string :as str]
   [criterium.util.format :as format]
   [criterium.viewer.common.core :as core]))

;;; Call site and object type formatting

(defn format-call-site
  "Format a call site map for display.
  Returns a string like 'class.method (file:line)'.
  When call-method is nil (object-type fallback), shows just the class.
  When call-site is not useful but object-types provided, shows those."
  ([call-site]
   (format-call-site call-site nil))
  ([{:keys [call-class call-method call-file call-line]} object-types]
   (cond
     ;; Full call-site info available
     (and (seq call-class) (seq call-method))
     (str call-class "." call-method " (" call-file ":" call-line ")")

     ;; Object-type fallback (call-method is nil)
     (and (seq call-class) (nil? call-method))
     call-class

     ;; No useful info, show object-types if available
     (seq object-types)
     (str "[" (str/join ", " (sort object-types)) "]")

     ;; Last resort
     :else
     (str call-class "." call-method " (" call-file ":" call-line ")"))))

(defn format-object-types
  "Format object types set for display.
  Returns a comma-separated string of simplified type names."
  [object-types]
  (when (seq object-types)
    (->> object-types
         sort
         (str/join ", "))))

;;; ASCII Treemap rendering

;;; Treemap box-drawing constants

(def ^:private ^String tree-branch
  "Branch connector for non-last children: ├── "
  "\u251C\u2500\u2500 ")

(def ^:private ^String tree-last
  "Last child connector: └── "
  "\u2514\u2500\u2500 ")

(def ^:private ^String tree-vertical
  "Vertical continuation line: │   "
  "\u2502   ")

(def ^:private ^String tree-space
  "Space continuation (after last child): 4 spaces"
  "    ")

(def ^:private ^String ellipsis
  "Ellipsis for truncated names: …"
  "\u2026")

(defn- render-treemap-node
  "Recursively render a treemap node.
  Returns a vector of lines."
  [node prefix is-last? max-value opts depth]
  (let [{:keys [^long bar-width
                ^long name-width
                depth-limit
                ^double min-percent]}
        opts
        depth (long depth)
        {:keys [name value children]} node
        is-leaf? (empty? children)
        connector (if is-last? tree-last tree-branch)
        continuation (if is-last? tree-space tree-vertical)
        node-name (if is-leaf? name (str name "/"))
        size-str (str "[" (format/format-value :memory value) "]")
        bar-str (when is-leaf?
                  (core/ascii-bar (double value) max-value bar-width))
        ;; Keep prefix + connector intact, only truncate the name if needed
        prefix-connector (str prefix connector)
        prefix-len (long (count prefix-connector))
        available-for-name (- name-width prefix-len)
        node-name-len (long (count node-name))
        ;; Truncate name from left if it exceeds available space
        ;; Ensure at least 2 chars available (for ellipsis + 1 char)
        truncated-name (cond
                         (<= available-for-name 1)
                         ellipsis

                         (> node-name-len available-for-name)
                         (str
                          ellipsis
                          (subs
                           node-name
                           (- node-name-len (dec available-for-name))))

                         :else
                         node-name)
        truncated-name-len (long (count truncated-name))
        ;; Pad to fill remaining space
        padding-needed (max 0 (- available-for-name truncated-name-len))
        padded-line (str prefix-connector truncated-name
                         (when (pos? padding-needed)
                           (apply str (repeat padding-needed \space))))
        line (str padded-line " " size-str
                  (when (seq bar-str) (str " " bar-str)))
        current-line [line]
        ;; Recurse into children if not at depth limit
        child-prefix (str prefix continuation)
        at-depth-limit? (and depth-limit (>= depth (long depth-limit)))]
    (if (or is-leaf? at-depth-limit?)
      current-line
      (let [root-value (double (:root-value opts))
            filtered-children (->> children
                                   (filter (fn [child]
                                             (>=
                                              (*
                                               100.0
                                               (/ (double (:value child))
                                                  root-value))
                                              min-percent)))
                                   (sort-by :value >))
            num-children (long (count filtered-children))]
        (into current-line
              (mapcat (fn [idx child]
                        (render-treemap-node
                         child
                         child-prefix
                         (= (long idx) (dec num-children))
                         max-value
                         opts
                         (inc depth)))
                      (range)
                      filtered-children))))))

(defn render-ascii-treemap
  "Render an allocation treemap as an ASCII tree string.

  treemap-data should be a :criterium/allocation-treemap map with :root, :group-by, :size-by.

  Options:
    :bar-width   - max bar characters (default 20)
    :depth-limit - max nesting depth to display, nil = unlimited (default nil)
    :min-percent - hide nodes below this % of root total (default 1)
    :name-width  - column width for names (default 40)"
  ([treemap-data] (render-ascii-treemap treemap-data {}))
  ([treemap-data opts]
   (let [{:keys [root group-by size-by]} treemap-data
         {:keys [bar-width depth-limit min-percent name-width]
          :or {bar-width 20 min-percent 1.0 name-width 40}} opts
         ^long name-width name-width]
     (if (nil? root)
       ""
       (let [root-value (double (:value root))
             max-leaf-value (if (empty? (:children root))
                              root-value
                              (->> (tree-seq :children :children root)
                                   (remove :children)
                                   (map :value)
                                   (reduce max 0.0)
                                   double))
             size-by-str (case size-by
                           :bytes "bytes"
                           :count "count"
                           :bytes-per-allocation "bytes/alloc"
                           (name (or size-by :bytes)))
             group-by-str (case group-by
                            :class→line→type "class→line→type"
                            :type→class→line "type→class→line"
                            (name (or group-by :class→line→type)))
             header (str "Allocation Treemap (by " size-by-str ", " group-by-str ")")
             root-name (str (:name root) "/")
             root-size (str "[" (format/format-value :memory root-value) "]")
             ;; Format root line with same fixed-width treatment as children
             root-name-len (long (count root-name))
             root-line (let [padded (cond
                                      (< root-name-len name-width)
                                      (str root-name
                                           (apply str (repeat (- name-width root-name-len) \space)))

                                      (> root-name-len name-width)
                                      (str ellipsis (subs root-name (- root-name-len (dec name-width))))

                                      :else
                                      root-name)]
                         (str padded " " root-size))
             render-opts {:bar-width bar-width
                          :depth-limit depth-limit
                          :min-percent (double min-percent)
                          :name-width name-width
                          :root-value root-value}
             children (:children root)
             filtered-children (->> children
                                    (filter (fn [child]
                                              (>= (* 100.0 (/ (double (:value child))
                                                              root-value))
                                                  (double min-percent))))
                                    (sort-by :value >))
             num-children (long (count filtered-children))
             child-lines (mapcat (fn [^long idx child]
                                   (render-treemap-node
                                    child
                                    ""
                                    (= idx (dec num-children))
                                    max-leaf-value
                                    render-opts
                                    1))
                                 (range)
                                 filtered-children)]
         (str/join "\n" (into [header root-line] child-lines)))))))
