;;; Compute the Euclidian distance between two images
(ns overscore.recognition.classification.euclidian
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training)
  (:import java.awt.image.BufferedImage))

(defn euclidian-distance
  "Compute the (squared) Euclidian image between two images. Assume
  every element in the training set has a size of 20x20"
  [^BufferedImage image segment template]
  (let [data (resize-to-vector image segment)]
    (reduce + (map #(let [d (- (if %1 1 0) (if %2 1 0))] (* d d))
                   data (:data template)))))