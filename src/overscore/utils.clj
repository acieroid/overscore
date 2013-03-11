;;; Collection of utilities functions
(ns overscore.utils
  (:use clojure.java.io)
  (:import java.awt.image.BufferedImage
           java.awt.Color
           javax.imageio.ImageIO
           java.io.PushbackReader
           java.io.File))

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

(defn mean
  "Return the mean of all the elements in the collection"
  [coll]
  (let [sum (reduce + coll)
        n (count coll)]
    (/ sum n)))

(defn ifilter
  "Return the indexes of the elements in coll that satisfy
  pred. Similar to filter, but returns the indexes instead of the
  elements"
  [coll pred]
  (let [n (count coll)]
    (map first (filter (comp pred second)
                       (map list (range n) coll)))))

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
  "Apply f to each pixel of the first image, and set the corresponding
  pixel in the second image to the result of f. f takes as argument
  the source image and the rgb value of the pixel, and returns the new
  pixel rgb value"
  [^BufferedImage src ^BufferedImage dest f]
  (doseq [x (range (.getWidth src))
          y (range (.getHeight src))]
    (.setRGB dest x y (f (.getRGB src x y)))))

;; TODO: see extract-rgb
(defn extract-gray
  "Return the gray value of a pixel. Assume we already have a
  grayscale image (in fact, it returns the R value, which is the same
  as the G and B ones for a grayscale image)."
  [^BufferedImage img ^long pixel]
  (bit-and 0xFF pixel))

;; TODO: It would be more clean to extract the values using the
;; ColorModel of the BufferedImage, though it might affect
;; performance. As is, this function will work with (A)RGB images, but
;; won't with other encodings (RGBA, (A)BGR, ...). However, those
;; encodings don't seem to be widely spread (I don't know if they're
;; used at all)
(defn extract-rgb
  "Extract the R, G and B values of a pixel in an image, return them
  as a vector"
  [^BufferedImage img ^long pixel]
  [(bit-and 0xFF (bit-shift-right pixel 16))
   (bit-and 0xFF (bit-shift-right pixel 8))
   (bit-and 0xFF pixel)])

;; TODO: there is probably a faster way than copying pixel by pixel
(defn copy-image
  "Copy a BufferedImage, setting each pixel to the value returned by
  the given function and return the newly created image.

  The function passed as argument should take three parameters: the x
  and y position of the current pixel, and the data currently
  associated with it (eg. its RGB color), which depends on the type of
  the image."
  [^BufferedImage img f & {:keys [type]
                           :or {type BufferedImage/TYPE_BYTE_BINARY}}]
  (let [out (BufferedImage. (.getWidth img) (.getHeight img)
                            type)]
    (doseq [x (range (.getWidth img))
            y (range (.getHeight img))]
      (.setRGB out x y (f x y (.getRGB img x y))))
    out))

(defn resize-image-or-fill
  "Resize an image to a given size. Return the newly created image. If
  the image is smaller than the given size, it is only copied, and the
  rest of the pixels remains white."
  [^BufferedImage img width height & {:keys [type]
                                      :or {type BufferedImage/TYPE_BYTE_BINARY}}]
  (let [resized (BufferedImage. width height type)
        g (.createGraphics resized)]
    (when (or (< (.getWidth img) width)
              (< (.getHeight img) height))
      ;; fill the image in in white first
      (.setPaint g Color/WHITE)
      (.fill g (java.awt.geom.Rectangle2D$Double. 0 0 width height)))
    (.drawImage g img 0 0
                (if (> (.getWidth img) width)
                  width
                  (.getWidth img))
                (if (> (.getHeight img) height)
                  height
                  (.getHeight img)) nil)
    (.dispose g)
    resized))

(defn draw-vector
  "Draw a vector that represents a black & white image"
  [vec w h dest]
  (let [img (BufferedImage. w h BufferedImage/TYPE_BYTE_BINARY)]
    (loop [vec vec
           x 0
           y 0]
      (if (< y h)
        (if (< x w)
          (do
            (if (or
                 ;; Depending on where this function is called, the
                 ;; vector might contain booleans or doubles
                 (true? (first vec))
                 (and (number? (first vec)) (> 0.1 (first vec))))
              (.setRGB img x y -1)
              (.setRGB img x y 0))
            (recur (rest vec) (inc x) y))
          (recur vec 0 (inc y)))
        (ImageIO/write img "png" (File. dest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filesystem utilities ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn temp-name
  "Create a temporary file name like what is created for temp-file and
   temp-dir. Taken from the fs library."
  ([prefix] (temp-name prefix ""))
  ([prefix suffix]
     (format "%s%s-%s%s" prefix (System/currentTimeMillis)
             (long (rand 0x100000000)) suffix)))

(defn read-vector
  "Safely read a vector from a file"
  [file]
  (with-open [f (PushbackReader. (reader file))]
    (binding [*read-eval* false]
      (read f))))

(defn write-vector
  "Write a vector to a file"
  [file vec]
  (with-open [f (writer file)]
    (binding [*out* f]
      (println vec))))