;;; Define and load the training set data
(ns overscore.recognition.classification.training
  (:use overscore.utils
        overscore.recognition.segmentation.segment)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defrecord training-data [class width height data])

(def training-set (atom []))

(defn to-vector
  "Convert a BufferedImage to a one-dimension boolean vector, where
  the 1 values correspond to activated pixels (thus, black
  pixels). If a segment is given, only return the data corresponding
  to that segment"
  ([^BufferedImage img]
     (to-vector img (->segment 0 (.getWidth img) 0 (.getHeight img))))
  ([^BufferedImage img segment]
     (loop [x (:start-x segment)
            y (:start-y segment)
            res (transient [])]
       (if (< y (+ (:start-y segment) (segment-height segment)))
         (if (< x (+ (:start-x segment) (segment-width segment)))
           (recur (inc x) y
                  (conj! res (if (== (.getRGB img x y) -1)
                               0 ; white (not active)
                               1 ; black (active)
                               )))
           (recur 0 (inc y) res))
         (persistent! res)))))

(defn resize-to-vector
  "Similar to to-vector, but first resize the image to 20x20 image"
  ([^BufferedImage img]
     (to-vector (resize-image img 20 20)))
  ([^BufferedImage img segment]
     (to-vector
      (resize-image
       (.getSubimage img
                     (:start-x segment) (:start-y segment)
                     (segment-width segment) (segment-height segment))
       20 20))))

(defn load-training-set-images
  "Load the training set data from a given directory. The structure of
  the data directory should be as follow:

    - The directory contains subdirectories named as the class we
      need (eg g_clef/)

    - The subdirectories contains png image representing the class of
      this subdirectory (the name of the files is not important
      there)

  Store the data in training-set, stored as training-data records with
  the data element set as the result of calling the store function
  with the BufferedImage corresponding to the current image."
  [in & {:keys [store]
         :or {store resize-to-vector}}]
  (if (empty? @training-set)
    (let [dir (File. in)]
      (doseq [subdir (.listFiles dir)]
        (when (.isDirectory subdir)
          (doseq [file (.listFiles subdir)]
            (when (.isFile file)
              (let [file (File. (str in "/" (.getName subdir) "/" (.getName file)))
                    img (ImageIO/read file)
                    data (->training-data
                          (keyword (.getName subdir))
                          (.getWidth img)
                          (.getHeight img)
                          (store img))]
                (swap! training-set #(cons data %))))))))
    (println "Training set already loaded")))