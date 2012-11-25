;;; Tool to convert Audiveris' training set files (in XML) to bitmaps,
;;; usable by overscore

(ns overscore.tools.audiveris
  (:use [overscore.musicxml :only [parse-int]])
  (:require [clojure.xml :as xml])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defrecord run [x start length])

(def black 0x000000)
(def white 0xFFFFFF)

(defn extract-run
  "Extract one run from its XML representation"
  [run first-pos]
  (->run first-pos
         (parse-int (:start (:attrs run)))
         (parse-int (:length (:attrs run)))))

(defn extract-runs-from-section
  "Extract all the runs of an XML representation of a section"
  [section]
  (let [first-pos (parse-int (:first-pos (:attrs section)))]
    (map #(extract-run % first-pos)
         (:content section))))

(defn extract-all-runs
  "Extract all the runs from a XML file"
  [xml]
  (reduce (fn [acc sec] (concat acc (extract-runs-from-section sec)))
          '()
          (filter #(= (:tag %) :section)
                  (:content xml))))

(defn draw-run
  "Draw a run in an image"
  [run img]
  (doseq [y (range (:start run) (+ (:start run) (:length run)))]
    (.setRGB img (:x run) y white)))

(defn draw-runs
  "Draw all the runs from an XML representation into an image"
  [xml img]
  (let [runs (extract-all-runs xml)]
    (println "Processing" (count runs) "runs")
    (doseq [run runs]
      (draw-run run img))))

(defn to-image
  "Convert an XML representation to an image"
  [in out]
  (let [img (BufferedImage. 2000 2000 BufferedImage/TYPE_BYTE_BINARY)
        xml (xml/parse in)]
    (draw-runs xml img)
    (ImageIO/write img "png" (File. out))))

