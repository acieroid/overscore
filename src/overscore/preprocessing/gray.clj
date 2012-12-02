;;; Convert a color image to a grayscale image
(ns overscore.preprocessing.gray
  (:use overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn color->grayscale
  "Convert all the pixels in image img to grayscale pixels, using the
  formula that gives the luma component (Y') from the red (R),
  green (G) and blue (B) values (used by PAL and NTSC, see
  wikipedia:grayscale):

    Y' = 0.3R + 0.59G + 0.11B

  Returns the new image created (don't modify the parameter)"
  [^BufferedImage img]
  (let [out (BufferedImage. (.getWidth img) (.getHeight img)
                            BufferedImage/TYPE_BYTE_GRAY)]
    (doseq [x (range (.getWidth img))
            y (range (.getHeight img))]
      (let [[r g b] (extract-rgb img x y)
            grayval (+ (* 0.3 r) (* 0.59 g) (* 0.11 b))]
        (.setRGB out x y (long grayval))))
    out))
