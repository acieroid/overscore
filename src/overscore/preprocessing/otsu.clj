;;; Implements Otsu's method for image binarization
;;; Otsu's method is well presented on [1].
;;; [1] http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
(ns overscore.preprocessing.otsu
  (:import (java.awt.image BufferedImage)))

(defn square
  "Return x squared"
  [x]
  (* x x))

(defn get-grey
  "Return the grey value of a pixel. Assume we already have a
  grey-scale image (so we can only extract the B value, since it
  should be equal to R and G for a grey-scale image)"
  [^long rgb]
  (bit-and rgb 0xFF))

(defn build-histogram
  "Build the histogram (a 256 elements vector). The ith element of the
  vector represents the number of pixels whose grey value is equal to
  i. Vectors are used because they have faster element access than
  sequences. Ideally, we would need a structure with a O(1) access,
  but clojure doesn't seem to have one, vectors are in O(log(n)) (says
  the documentation). We use a transient vector for improved performance."
  [^BufferedImage img]
  (loop [hist (transient (apply vector (take 256 (repeat 0))))
         x 0 y 0]
    (cond
     (>= x (.getWidth img)) (recur hist 0 (inc y)) ; next line
     (>= y (.getHeight img)) (persistent! hist) ; we're done
     :else (recur
               ;; update histogram
               (let [index (get-grey (.getRGB img x y))]
                 (assoc! hist index (inc (nth hist index))))
             (inc x) y))))

(defn compute-parameters
  "Compute the parameters for pixels in the histogram for which the
  type of pixels 'type' (:black or :white), given a certain
  threshold t. The parameters computed are the weight factor (w), the
  mean (m). In the fastest version of Otsu's method, we don't need to
  compute the variance, so we don't compute it here. The parameters
  are returned as a pair"
  [type ^long t hist]
  (let [ntot (reduce + hist)
        ;; the pixel we're interested in
        elements (case type
                   :black (take t hist)
                   :white (drop t hist))
        elements-size (count elements)
        n (reduce + elements)
        ;; weight factor
        w (/ n ntot)
        ;; mean numerator
        m-num (reduce
               +
               (map * elements (case type
                                 :black (range elements-size)
                                 :white (range (- 255 elements-size) 255))))
        ;; mean
        ;; TODO: not sure how to handle the n = 0 case
        m (if (== n 0) 0 (/ m-num n))]
    [w m]))

(defn compute-between-class-variance
  "Compute the between class variance for a certain value of the threshold, t."
  [^long t hist]
  (let [[wb mb] (compute-parameters :black t hist)
        [ww mw] (compute-parameters :white t hist)]
    (* wb ww (square (- mb mw)))))

(defn maximize
  "Find the element of a collection for which the value f returns with
  it as argument is the maximum.
  For example, (maximize #(* -1 %) [1 2 3]) is 1"
  [f coll]
  (let [helper (fn helper [[x & xs]]
                 (if (empty? xs)
                   [x (f x)]
                   (let [[max-item max-value] (helper xs)
                         value (f x)]
                     (if (> value max-value)
                       [x value]
                       [max-item max-value]))))]
    (first (helper coll))))

;; TODO: the method we use could be improved. We don't need to
;; recalculate from scratch the parameters for each t, but we can
;; compute them iteratively. See the implementation given in [1].
(defn find-threshold
  "Find the threshold value that maximizes the between class variance
  given a certain histogram"
  [img]
  (let [hist (build-histogram img)]
    (maximize #(compute-between-class-variance % hist) (range 1 255))))