;;; Implements Otsu's method for image binarization
;;; Otsu's method is well presented on [1].
;;; [1] http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
(ns overscore.preprocessing.otsu
  (:import (java.awt.image BufferedImage)))

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