;;; Convert a color image to a grayscale image
(ns overscore.preprocessing.gray
  (:use overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn gray->rgb
  "Convert a gray pixel to its RGB representation. For example, a
  white pixel has a gray value of 0xFF. Its RGB representation is
  0xFFFFFF."
  [^long grayval]
  (+ grayval
     (bit-shift-left grayval 8)
     (bit-shift-left grayval 16)))

(defn color->grayscale
  "Convert all the pixels in image img to grayscale pixels, using the
  formula that gives the luma component (Y') from the red (R),
  green (G) and blue (B) values (used by PAL and NTSC, see
  wikipedia:grayscale):

    Y' = 0.3R + 0.59G + 0.11B

  This function modifies the pixels in the image and returns nothing"
  [^BufferedImage img]
  (doseq [x (range (.getWidth img))
          y (range (.getHeight img))]
    (let [rgb (.getRGB img x y)
          grayval (+ (* 0.3 (extract-r rgb))
                     (* 0.59 (extract-g rgb))
                     (* 0.11 (extract-b rgb)))]
      (.setRGB img x y (gray->rgb (long grayval))))))
