;;; Do the level-0 segmentation by finding sequence of connected black
;;; regions on the x-projection of a system (as in OpenOMR).
(ns overscore.segmentation.level0
  (:use overscore.proj
        overscore.utils)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defrecord segment [start end])

(defn find-basic-segments
  "Find the segments of black regions in a x-projection of an
  image. Return a list of pairs containing the start and end position
  of each segment"
  [proj]
  (loop [proj proj
         i 0
         segments []
         start -1]
    (if (empty? proj)
      (reverse segments)
      (if (> (first proj) 0)
        ;; this column has black pixels
        (recur (rest proj) (inc i) segments
               (if (= start -1)
                 i ; new region
                 start))
        ;; no black pixels
        (if (= start -1)
          (recur (rest proj) (inc i) segments start)
          ;; end of region
          (recur (rest proj) (inc i)
                 (cons (->segment start i) segments)
                 -1))))))

(defn segment-size
  "Return the size of a segment"
  [segment]
  (- (:end segment) (:start segment)))

(defn improve-segments
  "Improve the segments found by find-basic-segments in two way:

    1. Group close segments, which can occur for example for a dotted
       note. Two segments closer than min-closeness are merged into one

    2. Drop small segments which are probably due to noise or vertical
       lines. The segments dropped are those which have a size smaller
       than min-size"
  [segments min-size min-closeness]
  (let [grouped (reverse
                 (reduce (fn
                           ([] []) ; no segments
                           ([st seg]
                              (if (empty? st)
                                [seg]
                                (let [[last & tail] st]
                                  (if (< (- (:start seg) (:end last)) min-closeness)
                                    (cons (->segment
                                           (:start last) (:end seg))
                                          tail) ; merge
                                    (cons seg st))))))
                         [(first segments)] segments))
        filtered (filter #(> (segment-size %) min-size) grouped)]
    filtered))

(defn color-level0-segments
  "Output a image where the level0 segments are colored, for debugging"
  [^BufferedImage img segments]
  (let [out (copy-image img
                        (fn [x y bw]
                          (if (= bw -1)
                            0xFFFFFF
                            0x0))
                        :type BufferedImage/TYPE_INT_RGB)]
    (doseq [segment segments
            x (range (:start segment) (:end segment))
            y (range (.getHeight out))]
      (.setRGB out x y (if (= (.getRGB img x y) -1)
                         0xAAFFAA ; color white in green in segments
                         0x0)))
    (ImageIO/write out "png" (File. "/tmp/level0-segments-debug.png"))))

(defn level0-segments
  "Return a sequence of level 0 segments (containing the start and end
  position of each segment, as a pair), given an image containing only
  one system."
  [^BufferedImage img debug & {:keys [min-size min-closeness]
                               :or {min-size 8
                                    min-closeness 5}}]
  (let [data (projection img :x)
        segments (improve-segments (find-basic-segments data)
                                   min-size min-closeness)]
    (when debug
      (color-level0-segments img segments))
    segments))

