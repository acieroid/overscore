(ns overscore.recognition.classification.classification
  (:use overscore.utils
        overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.opencv-knn)
  (:import javax.imageio.ImageIO
           java.io.File))

(defn classification
  "Classify all the symbols found by the segmentation process in an
  image"
  [training-set-path in-img in-segments out]
  (let [img (ImageIO/read (File. in-img))
        _ (load-training-set-images training-set-path)
        _ (create-knn)
        segments-vectors (read-vector in-segments)
        segments (map (fn [[sx sy ex ey]]
                        (->segment sx sy ex ey))
                      segments-vectors)
        classes (map #(classify-opencv-knn img %) segments)
        output (map (fn [seg class] [(:start-x seg)
                                     (:start-y seg)
                                     (:end-x seg)
                                     (:end-y seg)
                                     class])
                    segments classes)]
    (write-vector out output)))