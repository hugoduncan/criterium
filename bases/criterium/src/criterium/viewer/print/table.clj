(ns criterium.viewer.print.table
  "Print viewer table formatting.

  Provides generic table printing with box-drawing separators.")

(set! *unchecked-math* false)

(defn print-table
  "Print a formatted table with box-drawing separators.

  Takes a table specification map with:
    :heading     - optional heading string printed before the table
    :indent      - number of spaces to indent (default 2)
    :row-key-col - optional {:header \"\" :width N} for leftmost row-key column
    :columns     - vector of {:header \"Header\" :width N :align :right|:left}
    :rows        - vector of row data, each row is a vector of cell strings
                   If :row-key-col specified, first element is the row key

  Cell alignment defaults to :right. Column widths are auto-calculated from
  headers and data if not specified.

  Example:
    (print-table
      {:heading \"My Table\"
       :row-key-col {:header \"\" :width 8}
       :columns [{:header \"A\"} {:header \"B\"}]
       :rows [[\"key1\" \"val1\" \"val2\"]
              [\"key2\" \"val3\" \"val4\"]]})"
  [{:keys [heading indent row-key-col columns rows]
    :or {indent 2}}]
  (when heading
    (println heading))
  (let [indent-str (apply str (repeat indent \space))
        ;; Calculate column widths if not provided
        columns (mapv
                 (fn [col-idx {:keys [header width] :as col}]
                   (let [data-col-idx (if row-key-col (inc col-idx) col-idx)
                         max-data-width (when (seq rows)
                                          (apply
                                           max
                                           0
                                           (map
                                            #(count
                                              (str (nth % data-col-idx "")))
                                            rows)))
                         calc-width (max (count header)
                                         (or max-data-width 0))]
                     (assoc col :width (or width calc-width))))
                 (range)
                 columns)
        row-key-col (when row-key-col
                      (let [{:keys [header width]} row-key-col
                            max-key-width (when (seq rows)
                                            (apply
                                             max
                                             0
                                             (map
                                              #(count (str (first %)))
                                              rows)))
                            calc-width (max (count (or header ""))
                                            (or max-key-width 0)
                                            8)]
                        (assoc row-key-col :width (or width calc-width))))
        ;; Format a cell with alignment
        format-cell (fn [{:keys [width align]} value]
                      (let [align (or align :right)
                            fmt (str "%" (when (= align :left) "-") width "s")]
                        (format fmt (or value ""))))
        ;; Print header row
        _ (do
            (print indent-str)
            (when row-key-col
              (print (format-cell row-key-col (:header row-key-col))))
            (doseq [[i col] (map-indexed vector columns)]
              (when (or row-key-col (pos? i))
                (print " │ "))
              (print (format-cell col (:header col))))
            (println))
        ;; Print separator line
        _ (do
            (print indent-str)
            (when row-key-col
              (print (apply str (repeat (:width row-key-col) "─"))))
            (doseq [[i col] (map-indexed vector columns)]
              (when (or row-key-col (pos? i))
                (print "─┼─"))
              (print (apply str (repeat (:width col) "─"))))
            (println))]
    ;; Print data rows
    (doseq [row rows]
      (print indent-str)
      (when row-key-col
        (print (format-cell row-key-col (first row))))
      (doseq [[i col] (map-indexed vector columns)]
        (let [data-idx (if row-key-col (inc i) i)]
          (when (or row-key-col (pos? i))
            (print " │ "))
          (print (format-cell col (nth row data-idx nil)))))
      (println))))
