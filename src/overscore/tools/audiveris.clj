;;; Tool to convert Audiveris' training set files (in XML) to bitmaps,
;;; usable by overscore
(ns overscore.tools.audiveris
  (:use overscore.utils)
  (:require [clojure.xml :as xml]
            [clojure.string :as string])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defrecord run [x y length orientation])

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
  (let [black 0x000000]
    (case (:orientation run)
      :horizontal
      (doseq [x (range (:x run) (+ (:x run) (:length run)))]
        (.setRGB img x (:y run) black))
      :vertical
      (doseq [y (range (:y run) (+ (:y run) (:length run)))]
        (.setRGB img (:x run) y black)))))

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
  (let [xs (sort #(compare (:x %1) (:x %2)) runs) ; runs sorted on x
        ys (sort #(compare (:y %1) (:y %2)) runs) ; runs sorted on y
        x (:x (first xs))
        y (:y (first ys))
        last-f (fn [orientation accessor runs]
                 (last (sort #(compare (+ (accessor %1) (:length %1))
                                       (+ (accessor %2) (:length %2)))
                             (filter #(= (:orientation %) orientation)
                                     runs))))
        last-x-run (last-f :horizontal :x runs)
        last-y-run (last-f :vertical :y runs)
        width (+ 1 (max
                    (if last-x-run
                      (- (+ (:x last-x-run) (:length last-x-run)) x)
                      0)
                    (- (:x (last xs)) x)))
        height (+ 1 (max
                     (if last-y-run
                       (- (+ (:y last-y-run) (:length last-y-run)) y)
                       0)
                     (- (:y (last ys)) y)))]
    [x y width height]))

(defn to-image
  "Convert an XML representation to an image"
  [in out]
  (let [white 0xFFFFFF
        xml (xml/parse (File. in))
        original-runs (extract-all-runs xml)
        [x y width height] (find-dimensions original-runs)

        runs (map #(adjust-run x y %) original-runs)
        img (BufferedImage. width height BufferedImage/TYPE_BYTE_BINARY)]
    (fill img white)
    (draw-runs runs img)
    (ImageIO/write img "png" (File. out))))

(defn extract-name
  "Extract the name of a symbol from its Audiveris' XML file name"
  [filename]
  ;; The filename is "SYMBOLNAME.NUMBER.xml". We want to extract "symbolname"
  (string/lower-case (first (string/split filename #"\." 2))))

(defn md5
  "Generate a md5 checksum for the given string"
  [token]
  (let [hash-bytes
         (doto (java.security.MessageDigest/getInstance "MD5")
               (.reset)
               (.update (.getBytes token)))]
       (.toString
         (new java.math.BigInteger 1 (.digest hash-bytes))
         16)))

(defn output-filename
  "Return a unique filename in a subdirectory of the directory out"
  [out name complete-path]
  (let [dirname (str out "/" (extract-name name))
        dir (File. dirname)]
    (if (not (.exists dir))
      (.mkdirs dir))
    (str dirname "/" (md5 complete-path) ".png")))

(defn convert
  "Convert all XML files in a directory (in) to PNG files in another directory (out)"
  [in out]
  (println "Processing" in)
  (let [dir (File. in)]
    (doseq [file (.listFiles dir)]
      (let [filename (str in "/" (.getName file))]
        (if (.isDirectory file)
          (convert filename out)
          (if (.endsWith (.getName file) "xml")
            (try
              (to-image filename (output-filename out (.getName file) filename))
              (catch Exception e
                (println e)
                (println "Failed:" filename (.getMessage e))))))))))