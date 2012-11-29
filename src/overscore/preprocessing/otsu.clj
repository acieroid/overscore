;;; Implements Otsu's method for image binarization
;;; Otsu's method is well presented on [1].
;;; [1] http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
(ns overscore.preprocessing.otsu
  (:use overscore.utils)
  (:import (java.awt.image BufferedImage)))

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
               (let [index (extract-gray img (.getRGB img x y))]
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
        [wf mf] (compute-parameters :white t hist)]
    (* wb wf (square (- mb mf)))))

;;; TODO: the threshold seems to be a little bit different (typically
;;; 1 or 2 units less) from the threshold that
;;; find-threshold-iterative finds
(defn find-threshold
  "Find the threshold value that maximizes the between class variance
  given a certain image"
  [^BufferedImage img]
  (let [hist (build-histogram img)]
    (maximize #(compute-between-class-variance % hist) (range 1 255))))

(defn find-threshold-iterative
  "Find the threshold value that maximizes the between class variance
   given a certain image. Compute the threshold in an iterative
   way. The performance are a bit better than find-threshold on small
   images (around 30% faster). On big images (like a 300dpi scanned
   partition), find-threshold seems to be faster (around 5% faster)."
  [^BufferedImage img]
  (let [hist (build-histogram img)
        sum (reduce + (map * hist (range 256)))
        total (reduce + hist)]
    (loop [t 1
           max-t 0 max 0
           wb 0 wf 0 sumb 0]
      (if (>= t 256)
        max-t                           ; we return the best threshold
        (let [sumb (+ sumb (* t (nth hist t))) ; new value for sumb
              wb (+ wb (nth hist t))    ; weight for the background
              wf (- total wb)           ; weight for the foreground
              mb (if (== wb 0) 0 (/ sumb wb)) ; mean for the background
              mf (if (== wf 0) 0 (/ (- sum sumb) wf)) ; mean for the foreground
              var (* wb wf (square (- mb mf))) ; between class variance
              ;; new values for max and max-t
              [max max-t] (if (> var max) [var t] [max max-t])]
          (recur (inc t) max-t max wb wf sumb))))))

(defn binarize
  "Binarize an image, by finding the global threshold t. Pixels whose
  grey value is greater than t will be set as black and the others
  will be set as white."
  [^BufferedImage img]
  (let [black 0x000000
        white 0xFFFFFF
        t (find-threshold img)]
    (apply-to-pixels img
                     #(if (< (extract-gray img %) t)
                        black
                        white))))