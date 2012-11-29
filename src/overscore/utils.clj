;;; Collection of utilities functions
(ns overscore.utils
  (:import java.awt.image.BufferedImage))

(defn parse-int
  "Parse an integer, possibly from a floating-point representation"
  [n]
  (try
    (int (Double. n))
    (catch Exception e
      (println "Error when parsing integer from" n ":" e)
      0)))

(defn square
  "Return x squared"
  [x]
  (* x x))

;;;;;;;;;;;;;;;;;;;;;;
;;; List utilities ;;;
;;;;;;;;;;;;;;;;;;;;;;

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

(defn one-each-two
  "Return an element every two elements for the argument. For
  example, (one-each-two [1 2 3 4]) returns [1 3]"
  [coll]
  (loop [l coll
         acc []]
    (if (empty? l)
      (reverse acc)
      (recur (rest (rest l)) (cons (first l) acc)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug utilities ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro debug
  "Print an optional message and the value of x, then returns x"
  ([x]
     `(let [res# ~x]
        (println res#)
        res#))
  ([msg x]
     `(let [res# ~x]
        (println ~msg res#)
        res#)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BufferedImage and pixel manipulation functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-to-pixels
  "Change each pixels according to the function f. f takes as argument
  the rgb value of the pixel, and returns the new pixel rgb value"
  [^BufferedImage img f]
  (doseq [x (range (.getWidth img))
          y (range (.getHeight img))]
    (.setRGB img x y (f (.getRGB img x y)))))

(defn get-grey
  "Return the grey value of a pixel. Assume we already have a
  grey-scale image (so we can only extract the B value, since it
  should be equal to R and G for a grey-scale image)"
  [^long rgb]
  (bit-and rgb 0xFF))

(defn extract-r
  "Extract the R value of a RGB representation"
  [^long rgb]
  (bit-shift-right (bit-and rgb 0xFF0000) 16))

(defn extract-g
  "Extract the G value of a RGB representation"
  [^long rgb]
  (bit-shift-right (bit-and rgb 0x00FF00) 8))

(defn extract-b
  "Extract the B value of a RGB representation"
  [^long rgb]
  (bit-and rgb 0x0000FF))

