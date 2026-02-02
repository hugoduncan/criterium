(ns criterium.viewer.common-charts.profile
  "Profile visualization charts for call trees, treemaps, and most-called methods.

  Provides Vega and Vega-Lite specs for:
  - Allocation treemaps showing memory usage by type
  - Call tree visualizations (hierarchical tree and flame chart)
  - Most-called method bar charts"
  (:require
   [clojure.string :as str]))

;;; Treemap helpers

(defn- flatten-treemap-node
  "Flatten a hierarchical treemap node into a sequence of flat records.
  Each record has :id, :parent, and :name keys for use with Vega stratify.
  Only leaf nodes get stats - parent sizes are computed by Vega's treemap transform."
  ([node] (flatten-treemap-node node nil []))
  ([node parent-id path]
   (let [node-name (:name node)
         node-id (if (empty? path)
                   "root"
                   (str/join "/" (conj path node-name)))
         current-path (conj path node-name)
         children (:children node)
         node-record (cond-> {:id node-id
                              :parent parent-id
                              :name node-name
                              :depth (count path)}
                       ;; Only set stats on leaf nodes
                       (not children) (assoc
                                       :value
                                       (:value node 0)
                                       :bytes
                                       (:bytes node)
                                       :count
                                       (:count node)
                                       :freed-bytes
                                       (:freed-bytes node 0)
                                       :freed-count
                                       (:freed-count node 0)))]
     (if children
       (cons node-record
             (mapcat #(flatten-treemap-node % node-id current-path) children))
       [node-record]))))

(defn treemap-vega-spec
  "Build a complete Vega spec for treemap visualization.

  Takes treemap-data (a :criterium/allocation-treemap map from analysis)
  and opts map containing display options.

  Parameters:
    treemap-data - The allocation treemap map with :root containing hierarchy
    opts - Display options:
      :width (default 700)
      :height (default 400)
      :color-scheme (default \"tableau10\")

  Returns a full Vega spec (not Vega-Lite) using:
    - stratify transform to build hierarchy from flat data
    - treemap transform with squarify tiling
    - rect marks sized by x0/x1/y0/y1
    - color by first-level category
    - tooltip on hover showing name, value (formatted bytes), path"
  [treemap-data opts]
  (let [width (or (:width opts) 700)
        height (or (:height opts) 400)
        color-scheme (or (:color-scheme opts) "tableau10")
        root (:root treemap-data)
        flat-data (when root (vec (flatten-treemap-node root)))]
    {:$schema "https://vega.github.io/schema/vega/v5.json"
     :width width
     :height height

     :data [{:name "tree"
             :values (or flat-data [])
             :transform
             [{:type "stratify"
               :key "id"
               :parentKey "parent"}
              {:type "treemap"
               :field "value"
               :sort {:field "value" :order "descending"}
               :method "squarify"
               :ratio 1.6
               :size [{:signal "width"} {:signal "height"}]
               :as ["x0" "y0" "x1" "y1" "depth" "children"]}]}
            {:name "nodes"
             :source "tree"
             :transform [{:type "filter"
                          :expr "datum.children"}]}
            {:name "leaves"
             :source "tree"
             :transform [{:type "filter"
                          :expr "!datum.children"}]}]

     :scales [{:name "color"
               :type "ordinal"
               :domain {:data "nodes"
                        :field "name"
                        :sort true}
               :range {:scheme color-scheme}}]

     :marks [;; Parent category rectangles (colored background)
             {:type "rect"
              :from {:data "nodes"}
              :encode
              {:enter
               {:fill {:scale "color" :field "name"}}
               :update
               {:x {:field "x0"}
                :y {:field "y0"}
                :x2 {:field "x1"}
                :y2 {:field "y1"}}}}
             ;; Leaf rectangles (white stroke, interactive with tooltip)
             {:type "rect"
              :from {:data "leaves"}
              :encode
              {:enter
               {:stroke {:value "#fff"}
                :strokeWidth {:value 1}}
               :update
               {:x {:field "x0"}
                :y {:field "y0"}
                :x2 {:field "x1"}
                :y2 {:field "y1"}
                :fill {:value "transparent"}
                :tooltip
                {:signal
                 (str "{'Type': datum.name, "
                      "'Bytes': format(datum.bytes, '~s'), "
                      "'Count': datum.count, "
                      "'Bytes/Alloc': format(datum.bytes / datum.count, '.1f'), "
                      "'Freed %': format(datum['freed-count'] / datum.count, '.1%'), "
                      "'Path': replace(replace(datum.id, /^[^/]+\\//, ''), /\\/[^/]+$/, '')}")}}
               :hover
               {:fill {:value "rgba(0,0,0,0.1)"}}}}]}))

;;; Call Tree helpers

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
      (-> (subs class-name (inc (long idx)))
          (str/replace "_" "-")
          (str/replace "BANG" "!")
          (str/replace "QMARK" "?")
          (str/replace "STAR" "*")
          (str/replace "PLUS" "+")
          (str/replace "GT" ">")
          (str/replace "LT" "<")
          (str/replace "EQ" "=")))))

(defn- extract-simple-class-name
  "Extract simple class name without package prefix.
  'java.util.HashMap' -> 'HashMap'"
  [class-name]
  (when class-name
    (if-let [idx (str/last-index-of class-name ".")]
      (subs class-name (inc (long idx)))
      class-name)))

(defn- call-tree-display-name
  "Get display name for a call tree node.
  For Clojure invoke methods, shows the function name extracted from the class.
  For Java methods, shows SimpleClassName.method (without package)."
  [class-name method]
  (if (and (clojure-invoke-method? method)
           (str/includes? (or class-name "") "$"))
    (or (extract-clojure-fn-name class-name)
        (str class-name "." method))
    (str (extract-simple-class-name (or class-name "<unknown>"))
         "."
         (or method "<unknown>"))))

(defn- flatten-call-tree-node
  "Flatten a hierarchical call tree node into a sequence of flat records.
  Each record has :id, :parent, :name, :call-count keys for use with Vega stratify.
  Unlike treemap, all nodes get call-count since we're showing the call hierarchy."
  ([node] (flatten-call-tree-node node nil []))
  ([node parent-id path]
   (when node
     (let [class-name (or (:class node) "<unknown>")
           method (or (:method node) "<unknown>")
           display-name (call-tree-display-name class-name method)
           node-name (str class-name "." method)
           node-id (if (empty? path)
                     "root"
                     (str/join "/" (conj path node-name)))
           current-path (conj path node-name)
           children (:children node)
           call-count (or (:call-count node) 0)
           node-record {:id node-id
                        :parent parent-id
                        :name display-name
                        :class class-name
                        :method method
                        :file (:file node)
                        :line (:line node)
                        :call-count call-count
                        :depth (count path)}]
       (if (seq children)
         (cons node-record
               (mapcat
                #(flatten-call-tree-node % node-id current-path)
                children))
         [node-record])))))

(defn call-tree-tree-vega-spec
  "Build a Vega spec for hierarchical tree visualization of call graph.

  Displays the call tree as a top-down tree diagram where node size represents
  call count. Uses Vega's tree layout for proper hierarchical positioning.

  Parameters:
    call-tree - The call tree map with :class, :method, :call-count, :children
    opts - Display options:
      :width (default 700)
      :height (default 500)

  Returns a full Vega spec with:
    - stratify transform to build hierarchy
    - tree layout for positioning
    - symbol marks sized by call count
    - link paths connecting parent to children
    - tooltip on hover showing method, call count, file:line"
  [call-tree opts]
  (let [width (or (:width opts) 700)
        height (or (:height opts) 500)
        flat-data (when call-tree (vec (flatten-call-tree-node call-tree)))
        max-calls (long (if (seq flat-data)
                          (apply max (map :call-count flat-data))
                          1))]
    {:$schema "https://vega.github.io/schema/vega/v5.json"
     :width width
     :height height
     :padding 5

     :data [{:name "tree"
             :values (or flat-data [])
             :transform
             [{:type "stratify"
               :key "id"
               :parentKey "parent"}
              {:type "tree"
               :method "tidy"
               :size [{:signal "width - 100"} {:signal "height - 100"}]
               :separation true
               :as ["x" "y" "depth" "children"]}
              ;; Offset to center in available space
              {:type "formula" :as "x" :expr "datum.x + 50"}
              {:type "formula" :as "y" :expr "datum.y + 50"}]}
            {:name "links"
             :source "tree"
             :transform
             [{:type "treelinks"}
              {:type "linkpath"
               :orient "vertical"
               :shape "diagonal"}]}]

     :scales [{:name "color"
               :type "ordinal"
               :domain {:data "tree" :field "class" :sort true}
               :range {:scheme "category20"}}
              {:name "size"
               :type "sqrt"
               :domain [1 max-calls]
               :range [100 2000]}]

     :marks [;; Links between nodes
             {:type "path"
              :from {:data "links"}
              :encode
              {:update
               {:path {:field "path"}
                :stroke {:value "#ccc"}
                :strokeWidth {:value 1.5}}}}
             ;; Nodes
             {:type "symbol"
              :from {:data "tree"}
              :encode
              {:enter
               {:size {:scale "size" :field "call-count"}
                :fill {:scale "color" :field "class"}
                :stroke {:value "#fff"}
                :strokeWidth {:value 1}}
               :update
               {:x {:field "x"}
                :y {:field "y"}
                :tooltip
                {:signal
                 (str "{"
                      "'Function': datum.name, "
                      "'Full': datum.class + '.' + datum.method, "
                      "'Calls': datum['call-count'], "
                      "'Location': datum.file ? (datum.file + ':' + datum.line) : 'unknown'"
                      "}")}}}}
             ;; Labels for nodes with high call counts
             {:type "text"
              :from {:data "tree"}
              :encode
              {:enter
               {:font {:value "sans-serif"}
                :fontSize {:value 10}
                :align {:value "center"}
                :baseline {:value "bottom"}}
               :update
               {:x {:field "x"}
                :y {:field "y" :offset -5}
                :text {:signal (str "datum['call-count'] > "
                                    (/ max-calls 10)
                                    " ? datum.name : ''")}
                :fillOpacity {:value 0.8}}}}]}))

(defn call-tree-flame-vega-spec
  "Build a Vega spec for flame chart visualization of call graph.

  Displays the call tree as a flame chart where horizontal width represents
  call count (not time). Each level shows methods called, with children
  stacked below their parent.

  Parameters:
    call-tree - The call tree map with :class, :method, :call-count, :children
    total-calls - Total call count for percentage calculation
    opts - Display options:
      :width (default 700)
      :height (default 400)

  Returns a full Vega spec with:
    - Custom flame layout computed in Clojure
    - rect marks with width proportional to call count
    - Color by class for visual grouping
    - tooltip showing method, call count, percentage"
  [call-tree total-calls opts]
  (let [width (long (or (:width opts) 700))
        height (long (or (:height opts) 400))
        width-d (double width)
        height-d (double height)
        row-height (double 24)
        total-calls-d (double (if (pos? (long total-calls)) total-calls 1))]
    ;; Type hints eliminate boxed math warnings in this tight loop
    (letfn [(compute-flame-data
              [node ^double x0 ^double node-width ^long depth]
              (when node
                (let [call-count (long (or (:call-count node) 0))
                      class-name (or (:class node) "<unknown>")
                      method (or (:method node) "<unknown>")
                      display-name (call-tree-display-name class-name method)
                      percentage (* 100.0 (/ (double call-count) total-calls-d))
                      x1 (+ x0 node-width)
                      node-record {:name display-name
                                   :class class-name
                                   :method method
                                   :file (:file node)
                                   :line (:line node)
                                   :call-count call-count
                                   :percentage percentage
                                   :depth depth
                                   :x0 x0
                                   :x1 x1
                                   :y0 (* (double depth) row-height)
                                   :y1 (* (double (inc depth)) row-height)}
                      children (:children node)
                      ;; Children are sized proportionally within parent's width
                      children-total (double
                                      (reduce
                                       +
                                       0
                                       (map #(or (:call-count %) 0) children)))]
                  (if (seq children)
                    (let [child-data
                          (loop [remaining children
                                 child-x x0
                                 acc []]
                            (if (empty? remaining)
                              acc
                              (let [child (first remaining)
                                    child-count (double
                                                 (or (:call-count child) 0))
                                    child-width (if (pos? children-total)
                                                  (*
                                                   node-width
                                                   (/
                                                    child-count
                                                    children-total))
                                                  0.0)
                                    child-results (compute-flame-data
                                                   child
                                                   child-x
                                                   child-width
                                                   (inc depth))]
                                (recur (rest remaining)
                                       (+ child-x child-width)
                                       (into acc child-results)))))]
                      (cons node-record child-data))
                    [node-record]))))]
      (let [flame-data (when call-tree
                         (vec (compute-flame-data call-tree 0.0 width-d 0)))
            max-depth (long (if (seq flame-data)
                              (apply max (map :depth flame-data))
                              0))
            computed-height (long
                             (Math/max
                              height-d
                              (* (double (inc max-depth)) row-height 1.2)))]
        {:$schema "https://vega.github.io/schema/vega/v5.json"
         :width width
         :height computed-height
         :padding 5

         :data [{:name "flame"
                 :values (or flame-data [])}]

         :scales [{:name "color"
                   :type "ordinal"
                   :domain {:data "flame" :field "class" :sort true}
                   :range {:scheme "category20"}}]

         :marks [{:type "rect"
                  :from {:data "flame"}
                  :encode
                  {:enter
                   {:stroke {:value "#fff"}
                    :strokeWidth {:value 0.5}}
                   :update
                   {:x {:field "x0"}
                    :x2 {:field "x1"}
                    :y {:field "y0"}
                    :y2 {:field "y1"}
                    :fill {:scale "color" :field "class"}
                    :tooltip
                    {:signal
                     (str "{"
                          "'Function': datum.name, "
                          "'Full': datum.class + '.' + datum.method, "
                          "'Calls': datum['call-count'], "
                          "'Percentage': format(datum.percentage, '.1f') + '%', "
                          "'Location': datum.file ? (datum.file + ':' + datum.line) : 'unknown'"
                          "}")}}
                   :hover
                   {:fill {:value "#ff6600"}}}}
                 ;; Labels for wider bars
                 {:type "text"
                  :from {:data "flame"}
                  :encode
                  {:enter
                   {:font {:value "sans-serif"}
                    :fontSize {:value 10}
                    :align {:value "left"}
                    :baseline {:value "middle"}
                    :fill {:value "#000"}}
                   :update
                   {:x {:signal "datum.x0 + 2"}
                    :y {:signal "(datum.y0 + datum.y1) / 2"}
                    ;; Only show text if bar is wide enough
                    :text {:signal "(datum.x1 - datum.x0) > 60 ? datum.name : ''"}
                    :limit {:signal "datum.x1 - datum.x0 - 4"}}}}]}))))

;;; Most-Called Bar Chart

(defn- most-called-display-name
  "Get display name for a most-called entry.
  For Clojure invoke methods, shows the function name.
  For Java methods, shows class.method."
  [class-name method]
  (if (and (clojure-invoke-method? method)
           (str/includes? (or class-name "") "$"))
    (or (extract-clojure-fn-name class-name)
        (str class-name "." method))
    (str (or class-name "<unknown>") "." (or method "<unknown>"))))

(defn most-called-vega-lite-spec
  "Build a Vega-Lite horizontal bar chart spec for most-called methods.

  Parameters:
    most-called-data - The :most-called analysis result map
    opts - Display options:
      :width (default 600)
      :height (default: computed from number of items)

  Returns a Vega-Lite spec with:
    - Horizontal bars showing call counts
    - Methods sorted by call count descending
    - Tooltip showing full class.method, calls, and location"
  [most-called-data opts]
  (let [width (or (:width opts) 600)
        methods (:most-called most-called-data)
        n (count methods)
        bar-height 20
        height (or (:height opts) (+ 50 (* n bar-height)))
        data (mapv (fn [{:keys [class method file line total-calls]}]
                     (let [display-name (most-called-display-name class method)
                           location (if (and file (pos? (long (or line 0))))
                                      (str file ":" line)
                                      "")]
                       {"method" display-name
                        "fullName" (str class "." method)
                        "calls" total-calls
                        "location" location}))
                   methods)]
    {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
     :width width
     :height height
     :data {:values data}
     :mark {:type "bar"}
     :encoding {:y {:field "method"
                    :type "nominal"
                    :title "Method"
                    :sort {:field "calls" :order "descending"}
                    :axis {:labelLimit 300}}
                :x {:field "calls"
                    :type "quantitative"
                    :title "Call Count"}
                :color {:field "calls"
                        :type "quantitative"
                        :scale {:scheme "blues"}
                        :legend nil}
                :tooltip [{:field "fullName" :type "nominal" :title "Full Name"}
                          {:field "calls" :type "quantitative" :title "Calls"}
                          {:field "location"
                           :type "nominal"
                           :title "Location"}]}}))
