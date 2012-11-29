;;; Implement the processing step of the OMR system.
(ns overscore.preprocessing.preprocessing
  (:use overscore.preprocessing.gray
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
  ;; Grayscale images have max. 8 bits per pixel (the gray level)
  (<= (.getPixelSize (.getColorModel img)) 8))

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
  (let [img (ImageIO/read (File. in))]
    ;; Convert to grayscale
    (if (not (is-grayscale img))
      (color->grayscale img))
    ;; Convert to binary
    (if (not (is-binary img))
      (binarize img))
    (let [references (rle img)]
      ;; TODO: write it as a 1-bit grayscale PNG
      (ImageIO/write img "png" (File. out-img))
      ;; TODO: have something in utils.clj to handle text file read/write
      (with-open [f (writer out-ref)]
        (.write f (str references))))))


