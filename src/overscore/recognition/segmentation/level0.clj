;;; Do the level-0 segmentation by finding sequence of connected black
;;; regions on the x-projection of a system (as in OpenOMR).
(ns overscore.recognition.segmentation.level0
  (:use overscore.proj
        overscore.recognition.segmentation.segment)
  (:import java.awt.image.BufferedImage))

(defn find-basic-segments
  "Find the segments of black regions in a x-projection of an
  image. Return a list of pairs containing the start and end position
  of each segment"
  [^BufferedImage img proj]
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
                 (cons (->segment start i 0 (dec (.getHeight img))) segments)
                 -1))))))

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
                                  (if (< (- (:start-x seg) (:end-x last))
                                         min-closeness)
                                    (cons (->segment
                                           (:start-x last) (:end-x seg)
                                           (:start-y seg) (:end-y seg))
                                          tail) ; merge
                                    (cons seg st))))))
                         [(first segments)] segments))
        filtered (filter #(> (segment-width %) min-size) grouped)]
    filtered))

(defn level0-segments
  "Return a sequence of level 0 segments (containing the start and end
  position of each segment, as a pair), given an image containing only
  one system."
  [^BufferedImage img debug & {:keys [min-size min-closeness]
                               :or {min-size 8
                                    min-closeness 5}}]
  (let [data (projection img :x)
        segments (improve-segments (find-basic-segments img data)
                                   min-size min-closeness)]
    (when debug
      (color-segments img segments
                      :outfile "/tmp/level0-segments-debug.png"
                      :color 0xAAFFAA))
    segments))

