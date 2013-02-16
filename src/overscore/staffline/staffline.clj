;;; Implement the staffline processing step of the OMR system
(ns overscore.staffline.staffline
  (:use overscore.staffline.identification
        overscore.staffline.removal
        overscore.utils
        clojure.java.io)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defn color-stafflines
  [^BufferedImage img positions name]
  (let [out (copy-image img
                        (fn [x y bw]
                          ;; convert binary pixels to RGB pixels
                          (if (= bw -1)
                            0xFFFFFF
                            0x0))
                        :type BufferedImage/TYPE_INT_RGB)]
    ;; Color the staffline pixels in red
    ;; TODO: do it for staffline-height pixels of height
    (doseq [y positions
            x (range (.getWidth out))]
      (.setRGB out x y 0xFF0000))
    ;; Save the image
    (ImageIO/write out "png" (File. (str name "-debug.png")))))

(defn get-name
  "Return the name for the image of the nth system found in the file
   name (eg. the first staff in foo.png will have the name foo-1.png"
  [name n]
  (let [[_ basename ext] (re-find #"([^\.]+)\.(.+)" name)]
    (str basename "-" n ".png")))

(defn staffline-processing
  "Performs:
     1. Identify and isolate the systems, each system in a different image
     2. On each system:
       2.1. Identify the stafflines positions, producing a text file
            containing the positions
       2.2. Remove the stafflines, producing a new image if stafflines were found
   If the input is img.png, the outputs are img-n.png and img-n.txt
   where n is an integer.

  When debug is set to true, will also save debug images in
  img-n-debug.png where the stafflines found will be highlighted"
  [in debug]
  (let [img (ImageIO/read (File. in))
        imgs (isolate-systems img)]
    (loop [imgs imgs
           i 0]
      (when (not (empty? imgs))
        (println "Removing stafflines on system #" i)
        (let [[nostaff pos] (remove-stafflines (first imgs))]
          (when debug
            (color-stafflines (first imgs) pos (str in "-" i)))
          (when (not (empty? pos))
            (ImageIO/write nostaff "png" (File. (get-name in i)))
            (write-vector (str in "-" i ".txt") pos)))
        (recur (rest imgs) (inc i))))))