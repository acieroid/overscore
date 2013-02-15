(ns overscore.recognition.classification.classification
  (:use overscore.tools.files
        overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.knn)
  (:import javax.imageio.ImageIO
           java.io.File))

(defn classification
  "Classify all the symbols found by the segmentation process in an
  image"
  [in-img in-segments out]
  (let [img (ImageIO/read (File. in-img))
        segments-vectors (read-vector in-segments)
        segments (map (fn [[sx sy ex ey]]
                        (->segment sx sy ex ey))
                      segments-vectors)
        classes (map #(let [x 0]
                        (classify img %)) segments)
        output (map (fn [seg class] [(:start-x seg)
                                     (:start-y seg)
                                     (:end-x seg)
                                     (:end-y seg)
                                     class])
                    segments classes)]
    (write-vector out output)))