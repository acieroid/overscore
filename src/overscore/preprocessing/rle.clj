;;; Identify the staffline height and staffspace height parameters
;;; using RLE
(ns overscore.preprocessing.rle
  (:use overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn column-rle
  "Return the run length encoding for a column x of an image img"
  [^BufferedImage img x]
  (let [white 0xFFFFFF
        black 0x000000]
    (loop [rle '()
           color white
           count 0
           y 0]
      (if (>= y (.getHeight img))
        (reverse (cons count rle))
        (let [cur-color (.getRGB img x y)]
          (if (== color cur-color)
            (recur rle color (inc count) (inc y))
            (recur (cons count rle) cur-color 0 (inc y))))))))

(defn most-common
  "Return the most common value in the collection coll."
  [coll]
  (loop [h (transient (hash-map))
         l coll]
    (if (empty? l)
      ;; return the key that has the maximum value associated with it
      (let [h (persistent! h)]
        (first (maximize second (map list (keys h) (vals h)))))
      (recur (assoc! h (first l)
                     (inc (get h (first l) 0)))
             (rest l)))))

(defn most-common-b-and-w-run
  "Return the most common black run and white run for a given RLE, as
  a pair"
  [rle]
  (let [black (most-common (one-each-two rle))
        white (most-common (one-each-two (drop 1 rle)))]
    [black white]))

(defn rle
  "Identify the parameters of the image. Returns the staffline height
  and staffspace height as a pair (staffline height first)"
  [^BufferedImage img]
  (let [values (map #(most-common-b-and-w-run (column-rle img %))
                    (range (.getWidth img)))
        black (most-common (map first values))
        white (most-common (map second values))]
    [black white]))