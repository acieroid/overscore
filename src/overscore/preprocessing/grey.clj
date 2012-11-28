;;; Convert a color image to a greyscale image
(ns overscore.preprocessing.grey
  (:use overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn grey->rgb
  "Convert a grey pixel to its RGB representation. For example, a
  white pixel has a grey value of 0xFF. Its RGB representation is
  0xFFFFFF."
  [^long greyval]
  (+ greyval
     (bit-shift-left greyval 8)
     (bit-shift-left greyval 16)))

(defn color->greyscale
  "Convert all the pixels in image img to greyscale pixels, using the
  formula that gives the luma component (Y') from the red (R),
  green (G) and blue (B) values (used by PAL and NTSC, see
  wikipedia:Greyscale):

    Y' = 0.3R + 0.59G + 0.11B

  This function modifies the pixels in the image and returns nothing"
  [^BufferedImage img]
  (doseq [x (range (.getWidth img))
          y (range (.getHeight img))]
    (let [rgb (.getRGB img x y)
          greyval (+ (* 0.3 (extract-r rgb))
                     (* 0.59 (extract-g rgb))
                     (* 0.11 (extract-b rgb)))]
      (.setRGB img x y (grey->rgb (long greyval))))))
