(ns criterium.viewer.common.ascii-chart
  "ASCII chart rendering for terminal-based visualization.

  Provides LTTB (Largest Triangle Three Buckets) downsampling and
  ASCII line/scatter plot rendering for use in :print and :pprint viewers.

  Main entry points:
  - `lttb-downsample` - reduce points while preserving visual shape
  - `render-chart` - render points as ASCII chart, returns vector of strings"
  (:require
   [criterium.util.format :as format]))

(set! *unchecked-math* :warn-on-boxed)

;;; LTTB Downsampling Algorithm

(defn- triangle-area
  "Calculate area of triangle formed by three points using cross product.
  Returns absolute area * 2 (sufficient for comparison)."
  ^double [[^double x1 ^double y1] [^double x2 ^double y2] [^double x3 ^double y3]]
  (Math/abs
   (- (* (- x1 x3) (- y2 y1))
      (* (- x1 x2) (- y3 y1)))))

(defn- bucket-average
  "Calculate average point of points in a bucket."
  [points]
  (let [n (count points)]
    (if (zero? n)
      [0.0 0.0]
      (let [^double sum-x (reduce (fn [^double acc [^double x _]] (+ acc x)) 0.0 points)
            ^double sum-y (reduce (fn [^double acc [_ ^double y]] (+ acc y)) 0.0 points)
            nd (double n)]
        [(/ sum-x nd) (/ sum-y nd)]))))

(defn lttb-downsample
  "Downsample points using Largest Triangle Three Buckets algorithm.

  LTTB preserves the visual shape of the data by selecting points that
  form the largest triangles, keeping visually significant features.

  Parameters:
    points - sequence of [x y] coordinate pairs (must be sorted by x)
    target-count - desired number of output points

  Returns:
    Vector of [x y] pairs with at most target-count points.
    If points count <= target-count, returns points unchanged.

  Algorithm:
    1. Always keep first and last points
    2. Divide middle points into (target-count - 2) buckets
    3. For each bucket, select point forming largest triangle with
       previous selected point and average of next bucket"
  [points ^long target-count]
  (let [points (vec points)
        n (count points)]
    (cond
      (<= n target-count) points
      (<= target-count 2) [(first points) (last points)]
      :else
      (let [bucket-size (/ (double (- n 2)) (double (- target-count 2)))]
        (loop [result (transient [(first points)])
               i 0
               prev-selected (first points)]
          (if (>= i (- target-count 2))
            (persistent! (conj! result (last points)))
            (let [;; Current bucket range
                  bucket-start (long (+ 1 (* i bucket-size)))
                  bucket-end (long (min (+ 1 (* (inc i) bucket-size)) (dec n)))
                  ;; Next bucket for average calculation
                  next-start bucket-end
                  next-end (long (min (+ 1 (* (+ i 2) bucket-size)) n))
                  next-avg (bucket-average (subvec points next-start next-end))
                  ;; Find point in current bucket with largest triangle
                  best-point
                  (reduce
                   (fn [best idx]
                     (let [point (points idx)
                           area (triangle-area prev-selected point next-avg)]
                       (if (> area (double (first best)))
                         [area point]
                         best)))
                   [Double/NEGATIVE_INFINITY nil]
                   (range bucket-start bucket-end))
                  selected (second best-point)]
              (recur (conj! result selected) (inc i) selected))))))))

;;; Axis Label Formatting

(defn- compute-axis-labels
  "Compute axis labels with SI unit scaling.

  Parameters:
    min-val - minimum value on axis
    max-val - maximum value on axis
    dimension - :time, :memory, or nil for plain numbers
    num-labels - target number of labels

  Returns:
    {:labels [formatted-strings...]
     :positions [0.0-1.0 positions...]
     :unit string-or-nil}"
  [^double min-val ^double max-val dimension ^long num-labels]
  (if (= min-val max-val)
    {:labels [(format "%.3g" min-val)]
     :positions [0.5]
     :unit nil}
    (let [[^double scale unit] (if dimension
                                 (format/scale dimension (/ (+ min-val max-val) 2.0))
                                 [1.0 nil])
          range-val (- max-val min-val)
          step (/ range-val (double (max 1 (dec num-labels))))
          labels (mapv
                  (fn [^long i]
                    (let [val (+ min-val (* i step))
                          scaled (* val scale)]
                      (format "%.3g" scaled)))
                  (range num-labels))
          positions (mapv
                     (fn [^long i]
                       (/ (double i) (double (max 1 (dec num-labels)))))
                     (range num-labels))]
      {:labels labels
       :positions positions
       :unit unit})))

(defn- max-label-width
  "Calculate maximum width of labels."
  ^long [labels]
  (reduce (fn [^long acc s] (max acc (count s))) 0 labels))

;;; Chart Grid Rendering

(defn- map-to-grid
  "Map a value from data range to grid position.
  Returns position in [0, size-1] range."
  ^long [^double val ^double min-val ^double max-val ^long size]
  (if (= min-val max-val)
    (quot size 2)
    (let [normalized (/ (- val min-val) (- max-val min-val))
          pos (* normalized (double (dec size)))]
      (max 0 (min (dec size) (Math/round pos))))))

(defn- create-grid
  "Create a 2D character grid initialized with spaces.
  Grid is indexed as [row][col] where row 0 is top."
  [^long width ^long height]
  (vec (repeat height (vec (repeat width \space)))))

(defn- grid-set
  "Set a character in the grid. Row 0 is top of chart."
  [grid ^long row ^long col ch]
  (if (and (>= row 0) (< row (count grid))
           (>= col 0) (< col (count (first grid))))
    (assoc-in grid [row col] ch)
    grid))

(defn- plot-point
  "Plot a single point on the grid.
  Y is inverted: high values at top (low row index)."
  [grid x y plot-height point-char]
  (let [x (long x)
        y (long y)
        plot-height (long plot-height)
        row (- (dec plot-height) y)]
    (grid-set grid row x point-char)))

(defn- plot-line-segment
  "Plot a line segment between two points using Bresenham's algorithm."
  [grid [x0 y0] [x1 y1] plot-height line-char]
  (let [x0 (long x0)
        y0 (long y0)
        x1 (long x1)
        y1 (long y1)
        plot-height (long plot-height)
        dx (long (Math/abs (- x1 x0)))
        dy (long (Math/abs (- y1 y0)))
        sx (long (if (< x0 x1) 1 -1))
        sy (long (if (< y0 y1) 1 -1))]
    (loop [grid grid
           x x0
           y y0
           err (- dx dy)]
      (let [grid (plot-point grid x y plot-height line-char)]
        (if (and (= x x1) (= y y1))
          grid
          (let [e2 (* 2 err)
                step-x? (> e2 (- dy))
                step-y? (< e2 dx)
                new-err (cond-> err
                          step-x? (- dy)
                          step-y? (+ dx))
                new-x (if step-x? (+ x sx) x)
                new-y (if step-y? (+ y sy) y)]
            (recur grid new-x new-y new-err)))))))

;;; Main Rendering Functions

(defn render-chart
  "Render points as an ASCII chart.

  Parameters:
    points - sequence of [x y] coordinate pairs
    opts - options map:
      :width - total chart width in characters (default 80)
      :height - total chart height in lines (default 20)
      :x-label - x-axis label (optional)
      :y-label - y-axis label (optional)
      :dimension - :time or :memory for SI unit formatting (optional)
      :point-char - character for points (default \\*)
      :line-char - character for lines, nil to disable (default \\.)
      :title - chart title (optional)

  Returns:
    Vector of strings, one per line, representing the chart.
    Empty vector if points is empty or has fewer than 1 point."
  [points {:keys [^long width ^long height x-label y-label dimension
                  point-char line-char title]
           :or {width 80
                height 20
                point-char \*
                line-char \.}}]
  (if (empty? points)
    []
    (let [points (vec points)
          ;; Extract bounds
          xs (mapv first points)
          ys (mapv second points)
          x-min (apply min xs)
          x-max (apply max xs)
          y-min (apply min ys)
          y-max (apply max ys)

          ;; Compute y-axis labels (left side)
          num-y-labels (min 5 height)
          y-axis-info (compute-axis-labels y-min y-max dimension num-y-labels)
          y-label-width (max-label-width (:labels y-axis-info))
          y-axis-width (+ y-label-width 2) ; labels + " |"

          ;; Compute x-axis labels (bottom)
          x-axis-height 2 ; axis line + labels

          ;; Plot area dimensions
          plot-width (- width y-axis-width)
          plot-height (- height x-axis-height)

          ;; Downsample if needed
          points (if (> (count points) plot-width)
                   (lttb-downsample points plot-width)
                   points)

          ;; Map points to grid coordinates
          grid-points (mapv
                       (fn [[x y]]
                         [(map-to-grid x x-min x-max plot-width)
                          (map-to-grid y y-min y-max plot-height)])
                       points)

          ;; Create and populate grid
          grid (create-grid plot-width plot-height)

          ;; Plot lines between consecutive points
          grid (if line-char
                 (reduce
                  (fn [g [p1 p2]]
                    (plot-line-segment g p1 p2 plot-height line-char))
                  grid
                  (partition 2 1 grid-points))
                 grid)

          ;; Plot points (on top of lines)
          grid (reduce
                (fn [g [x y]]
                  (plot-point g x y plot-height point-char))
                grid
                grid-points)

          ;; Build output lines
          lines (transient [])

          ;; Add title if provided
          _ (when title
              (conj! lines title))

          ;; Build chart lines with y-axis
          y-label-positions (zipmap
                             (mapv (fn [^double pos]
                                     (Math/round (* pos (double (dec plot-height)))))
                                   (:positions y-axis-info))
                             (:labels y-axis-info))

          _ (doseq [row (range plot-height)]
              (let [y-grid-pos (- (dec plot-height) (long row))
                    y-label (get y-label-positions y-grid-pos "")
                    padded-label (format (str "%" y-label-width "s") y-label)
                    row-chars (apply str (grid row))]
                (conj! lines (str padded-label " |" row-chars))))

          ;; Add x-axis line
          x-axis-line (str (apply str (repeat y-axis-width \space))
                           (apply str (repeat plot-width \-)))
          _ (conj! lines x-axis-line)

          ;; Add x-axis labels
          num-x-labels (min 5 (quot plot-width 10))
          x-axis-info (compute-axis-labels x-min x-max nil num-x-labels)
          x-labels-line (let [sb (StringBuilder.)
                              _ (.append sb (apply str (repeat y-axis-width \space)))
                              positions (vec (:positions x-axis-info))]
                          (doseq [[^long i label] (map-indexed vector (:labels x-axis-info))]
                            (let [pos (long (* ^double (positions i)
                                               (double (dec plot-width))))
                                  current-len (.length sb)
                                  target-pos (+ y-axis-width pos)
                                  padding (max 0 (- target-pos current-len))]
                              (when (pos? padding)
                                (.append sb (apply str (repeat padding \space))))
                              (.append sb label)))
                          (.toString sb))
          _ (conj! lines x-labels-line)

          ;; Add axis labels if provided
          _ (when (or x-label y-label (:unit y-axis-info))
              (let [unit-str (when (:unit y-axis-info)
                               (str "(" (:unit y-axis-info) ")"))
                    label-line (str (when y-label (str y-label " "))
                                    unit-str
                                    (when x-label
                                      (str (apply str (repeat (- (quot width 2) 10) \space))
                                           "x: " x-label)))]
                (when (seq label-line)
                  (conj! lines label-line))))]

      (persistent! lines))))

(defn render-chart-simple
  "Simplified chart rendering for quick visualizations.

  Takes just points and optional width, using sensible defaults.
  Returns vector of strings."
  ([points]
   (render-chart-simple points 80))
  ([points ^long width]
   (render-chart points {:width width :height (quot width 4)})))
