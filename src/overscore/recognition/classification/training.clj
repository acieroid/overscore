;;; Define and load the training set data
(ns overscore.recognition.classification.training
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(defrecord training-data [class image])

(def training-set (atom []))

(defn load-training-set
  "Load the training set data from a given directory. The structure of
  the data directory should be as follow:

    - The directory contains subdirectories named as the class we
      need (eg g_clef/)

    - The subdirectories contains png image representing the class of
      this subdirectory (the name of the files is not important
      there)"
  [in]
  (if (empty? @training-set)
    (let [dir (File. in)]
      (doseq [subdir (.listFiles dir)]
        (when (.isDirectory subdir)
          (doseq [file (.listFiles subdir)]
            (when (.isFile file)
              (let [file (File. (str in "/" (.getName subdir) "/" (.getName file)))
                    data (->training-data
                          (keyword (.getName subdir))
                          (ImageIO/read file))]
                (swap! training-set #(cons data %))))))))
    (println "Training set already loaded")))
