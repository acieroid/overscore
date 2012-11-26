;;; Tool to convert Audiveris' training set files (in XML) to bitmaps,
;;; usable by overscore

(ns overscore.tools.audiveris
  (:use [overscore.musicxml :only [parse-int]])
  (:require [clojure.xml :as xml])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defrecord run [x y length orientation])

(def black 0x000000)
(def white 0xFFFFFF)

(defn extract-run
  "Extract one run from its XML representation"
  [run orientation first-pos]
  (->run (case orientation
           :horizontal (parse-int (:start (:attrs run)))
           :vertical first-pos)
         (case orientation
           :horizontal first-pos
           :vertical (parse-int (:start (:attrs run))))
         (parse-int (:length (:attrs run)))
         orientation))

(defn parse-orientation
  "Return the orientation corresponding to its string representation"
  [s]
  (case s
    "HORIZONTAL" :horizontal
    "VERTICAL" :vertical))

(defn extract-runs-from-section
  "Extract all the runs of an XML representation of a section"
  [section]
  (let [first-pos (parse-int (:first-pos (:attrs section)))
        orientation (parse-orientation (:orientation (:attrs section)))]
    (map #(extract-run %1 orientation (+ first-pos %2))
         (:content section)
         (iterate inc 0))))

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
  (case (:orientation run)
    :horizontal
    (doseq [x (range (:x run) (+ (:x run) (:length run)))]
      (.setRGB img x (:y run) black))
    :vertical
    (doseq [y (range (:y run) (+ (:y run) (:length run)))]
      (.setRGB img (:x run) y black))))

(defn draw-runs
  "Draw all the runs from an XML representation into an image"
  [runs img]
  (doseq [run runs]
    (draw-run run img)))

(defn adjust-run
  "Adjust the start position of a run"
  [x y run]
  (->run (- (:x run) x)
         (- (:y run) y)
         (:length run)
         (:orientation run)))

(defn fill
  "Fill an image with a certain color"
  [img color]
  (doseq [x (range (.getWidth img))
          y (range (.getHeight img))]
    (.setRGB img x y color)))

(defn find-dimensions
  "Find the dimensions of an image given its runs"
  [runs]
  (let [xs (map :x (sort #(compare (:x %1) (:x %2)) runs))
        x (first xs)
        width (+ 1 (- (last xs) x))
        y (:y (first (sort #(compare (:y %1) (:y %2)) runs)))
        last-run (last (sort #(compare (+ (:y %1) (:length %1))
                                       (+ (:y %2) (:length %2)))
                             runs))
        height (+ 1 (- (+ (:y last-run) (:length last-run)) y))]
    [x y width height]))

(defn to-image
  "Convert an XML representation to an image"
  [in out]
  (let [xml (xml/parse in)
        original-runs (extract-all-runs xml)
        [x y width height] (find-dimensions original-runs)

        runs (map #(adjust-run x y %) original-runs)
        img (BufferedImage. width height BufferedImage/TYPE_BYTE_BINARY)]
    (fill img white)
    (draw-runs runs img)
    (ImageIO/write img "png" (File. out))))

