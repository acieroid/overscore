;;; Tools to do horizontal/vertical projections of black pixels
(ns overscore.proj
  (:import java.awt.image.BufferedImage))

(defn black-pixel?
  "Is a pixel of an image black?"
  [^BufferedImage img ^long x ^long y]
  ;; We *should* be manipulating a binary image, so we only need the last bit
  (== (bit-and 0x1 (.getRGB img x y)) 0))

(defn count-pixels
  "Count the number of black pixels on a row/column described by its x
  and y values. An example of call is (count-pixels img [0] (range
  10)), for column 0 (fixed x)"
  [^BufferedImage img xs ys]
  (loop [cur-xs xs cur-ys ys count 0]
    (cond (empty? cur-ys) count
          (empty? cur-xs) (recur xs (rest cur-ys) count)
          :else (recur (rest cur-xs) cur-ys
                       (if (black-pixel? img (first cur-xs) (first cur-ys))
                         (inc count)
                         count)))))

(defn projection
  "Return the horizontal/vertical projection of black pixels for an
  image. Direction is either :x or :y, indicating the axis on which to
  project."
  [^BufferedImage img direction]
  (let [xs (range (.getWidth img))
        ys (range (.getHeight img))]
    (case direction
      :x
      (map #(count-pixels img [%] ys) xs)
      :y
      (map #(count-pixels img xs [%]) ys))))
