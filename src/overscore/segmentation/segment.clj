;;; Defines the data structure that will represent the segments
(ns overscore.segmentation.segment
  (:use overscore.utils)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defrecord segment [start-x end-x start-y end-y])

(defn segment-width
  "Return the width of a segment"
  [segment]
  (- (:end-x segment) (:start-x segment)))

(defn segment-height
  "Return the height of a segment"
  [segment]
  (- (:end-y segment) (:start-y segment)))

(defn random-color
  []
  (long (rand 0xFFFFFF)))

(defn color-segments
  "Output a image where the given segments are colored, for debugging"
  [^BufferedImage img segments & {:keys [outfile color other-color
                                         black other-black]
                                  :or {outfile "/tmp/segments.png"
                                       color 0xAAFFAA
                                       other-color false
                                       black 0x0
                                       other-black false}}]
  (let [out (copy-image img
                        (fn [x y bw]
                          (if (= bw -1)
                            0xFFFFFF
                            0x0))
                        :type BufferedImage/TYPE_INT_RGB)]
    (doall
     (map (fn [segment color black]
            (let [black (random-color)]
              (doseq [x (range (:start-x segment) (inc (:end-x segment)))
                      y (range (:start-y segment) (inc (:end-y segment)))]
                (.setRGB out x y
                         (if (= (.getRGB img x y) -1)
                           color    ; color white in 'color' in segments
                           black)))))
          segments
          (if other-color
            (cycle [color other-color])
            (repeat color))
          (if other-black
            (cycle [black other-black])
            (repeat black))))
    (ImageIO/write out "png" (File. outfile))))
