;;; Remove the staff lines and identify their position.

;;; It uses the python script removal.py
(ns overscore.staffline.removal
  (:use overscore.utils
        clojure.java.io
        [clojure.java.shell :only [sh]])
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

;; The python implementation of the system
(def python "python2")
;; The removal script name
(def script "src/overscore/staffline/removal.py")

(defn remove-stafflines
  "Remove the stafflines from the file img-in. Return the
  image and the stafflines position as a pair [img-out pos]."
  [^BufferedImage img-in]
  (let [in (temp-name "overscore-omr-in")
        out (temp-name "overscore-omr-out")
        pos (temp-name "overscore-omr-pos")]
    (ImageIO/write img-in "png" (File. in))
    (sh python script in out pos)
    (let [img-out (ImageIO/read (File. out)) ; output image
          pos-out (read-string (slurp pos))] ; staffline positions
      (delete-file in)
      (delete-file out)
      (delete-file pos)
      [img-out pos-out])))