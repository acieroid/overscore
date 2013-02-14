;;; Implement the prerprocessing step of the OMR system.
(ns overscore.preprocessing.preprocessing
  (:use overscore.tools.files
        overscore.preprocessing.gray
        overscore.preprocessing.otsu
        overscore.preprocessing.rle
        clojure.java.io)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defn is-binary
  "Is an image already binarized?"
  [img]
  ;; Binary images have only one bit per pixel
  (== (.getPixelSize (.getColorModel img)) 1))

(defn is-grayscale
  "Is an image already in grayscale?"
  [img]
  ;; Grayscale images have max. 8 bits per pixel (the gray level) and
  ;; only one component
  (let [cm (.getColorModel img)]
    (and
     (<= (.getPixelSize cm) 8)
     (== (count (.getComponentSize cm)) 1))))

(defn preprocessing
  "Performs:
     1. Color to grayscale conversion
     2. Binarization using Otsu's method
     3. Extract the reference lengths (staffline height and staffspace height)
   Input:
     - PNG image (~300dpi resolution should be good)
   Output:
     - Grayscale 1-bit PNG image (black and white)
     - Reference lengths as a pair (in a file)"
  [in out-img out-ref]
  (let [img (ImageIO/read (File. in))
        grayscale (if (is-grayscale img) img (color->grayscale img))
        binary (if (is-binary grayscale) grayscale (binarize grayscale))
        references (rle binary)]
    (ImageIO/write binary "png" (File. out-img))
    (write-vector f references)))


