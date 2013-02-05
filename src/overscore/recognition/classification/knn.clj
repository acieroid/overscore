;;; Classify symbols (described by a segment of an image) using
;;; k-Nearest-Neighbours algorithm
(ns overscore.recognition.classification.knn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.hausdorff)
  (:import java.awt.image.BufferedImage))

(defn classify
  "Classify a symbol contained in a segment of an image"
  [^BufferedImage img segment & {:keys [k distance]
                                 :or {k 3
                                      distance hausdorff-distance}}]
  (let [neighbours (take k (sort-by #(distance img segment %) training-set))
        most-frequent-class (first
                             (partial max-key second
                                      (frequencies
                                       (map :class neighbours))))]
    most-frequent-class))