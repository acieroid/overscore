(ns overscore.recognition.classification.knn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.knn)
  (:import javax.imageio.ImageIO
           java.io.File))

(defn classification
  "Classify all the symbols found by the segmentation process in an
  image"
  [in-img in-segments out]
  (let [img (ImageIO/read (File. in-img))
        segments-vectors (read-vectors in-segments)
        segments (map #(apply ->segment) segments-vectors)
        classes (map #(classify img %) segments)
        output (map (fn [seg class] [(:start-x seg)
                                     (:start-y seg)
                                     (:end-x seg)
                                     (:end-y seg)
                                     class])
                    segments classes)]
    (write-vectors out output)))