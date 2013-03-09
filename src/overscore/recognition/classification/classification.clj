(ns overscore.recognition.classification.classification
  (:use overscore.utils
        overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.opencv-knn)
  (:import javax.imageio.ImageIO
           java.awt.Color
           java.awt.image.BufferedImage
           java.io.File))

(defn draw-classes
  "Annotate an image with the class of each segment"
  [^BufferedImage img classes]
  (let [colored-img (copy-image img
                                (fn [x y bw] 
                                  (if (= bw -1) 
                                    0xFFFFFF
                                    0x0))
                                :type BufferedImage/TYPE_INT_RGB)
        g (.createGraphics colored-img)]
    (.setColor g Color/RED)
    (doseq [[start-x start-y end-x end-y class] classes]
      (.drawString g (str class)
                   start-x end-y))
    (ImageIO/write colored-img "png" (File. "/tmp/debug-classification.png"))))

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
    (draw-classes img output)
    (write-vector out output)))