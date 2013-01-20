;;; Implement the staffline processing step of the OMR system
(ns overscore.staffline.staffline
  (:use overscore.staffline.identification
        overscore.staffline.removal
        clojure.java.io)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defn staffline-processing
  "Performs:
     1. Identify and isolate the systems, each system in a different image
     2. On each system:
       2.1. Identify the stafflines positions, producing a text file
            containing the positions
       2.2. Remove the stafflines, producing a new image if stafflines were found
   If the input is img.png, the outputs are img-n.png and img-n.txt
   where n is an integer."
  [in]
  (let [img (ImageIO/read (File. in))
        imgs (isolate-systems img)]
    (loop [imgs imgs
           i 0]
      (when (not (empty? imgs))
        (let [[nostaff pos] (remove-stafflines (first imgs))]
          (when (not (empty? pos))
            (ImageIO/write nostaff "png" (File. (str in "-" i ".png")))
            (with-open [f (writer (str in "-" i ".txt"))]
              (.write f (str pos)))))
        (recur (rest imgs) (inc i))))))