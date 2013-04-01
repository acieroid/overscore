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

(defn evaluate-classification-method
  "Evaluate a classification method m (which takes an training set data as input),
  using the cross validation set cvset. Return for each symbol class
  the precision, the recall, and the F1-score"
  [m cvset]
  (let [data (map (fn [cvelem] [(m (:data cvelem)) (:class cvelem)]) cvset)
        update (fn [data key key-f f]
                 (map (fn [[x val]]
                        (if (= x key)
                          [x (apply key-f val)]
                          [x (apply f val)]))
                      data))]
    (loop [data data
           results (map
                    ;; true negative ---------.
                    ;; false negative ------. |
                    ;; false positive ----. | |
                    ;; true positive ---. | | |
                    ;;                  v v v v
                    (fn [class] [class [0 0 0 0]])
                    (keys (group-by (fn [x] x) (map :class @training-set))))]
      (println (first data))
      (if (empty? data)
        (map (fn [[class [tp fp fn tn]]]
               (let [P (if (== (+ tp fp) 0)
                         :inf
                         (/ tp (+ tp fp)))
                     R (if (== (+ tp fn) 0)
                         :inf
                         (/ tp (+ tp fn)))
                     F1 (if (or (= P :inf) (= R :inf)
                                (== P 0) (== R 0))
                          0
                          (* 2 (/ (* P R) (+ P R))))]
                 [class [P R F1]]))
             results)
        (let [[found real] (first data)]
          (recur (rest data)
                 (if (= found real)
                   ;; Correct
                   (update results
                           found (fn [tp fp fn tn] [(inc tp) fp fn tn])
                           (fn [tp fp fn tn] [tp fp fn (inc tn)]))
                   (if (= found :none)
                     ;; Incorrect -- no symbol
                     (update results
                             real (fn [tp fp fn tn] [tp fp (inc fn) tn])
                             (fn [tp fp fn tn] [tp fp fn (inc tn)]))
                     ;; Incorrect -- other symbol
                     (update
                      (update results
                              real (fn [tp fp fn tn] [tp fp (inc fn) tn])
                              (fn [tp fp fn tn] [tp fp fn (inc tn)]))
                      found (fn [tp fp fn tn] [tp (inc fp) fn tn])
                      (fn [tp fp fn tn] [tp fp fn tn]))))))))))

(defn classification
  "Classify all the symbols found by the segmentation process in an
  image"
  [training-set-path in-img in-segments out]
  (let [img (ImageIO/read (File. in-img))
        _ (load-training-set-images training-set-path)
        _ (create-knn @training-set)
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
