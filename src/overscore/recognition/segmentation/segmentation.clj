;;; Do the whole segmentation process
(ns overscore.recognition.segmentation.segmentation
  (:use overscore.tools.files
        overscore.recognition.segmentation.segment
        overscore.recognition.segmentation.level0
        overscore.recognition.segmentation.notehead
        overscore.recognition.segmentation.level1
        overscore.recognition.segmentation.level2)
  (:import java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

;;; TODO: drop the empty, really tiny, and blank segments
(defn find-segments
  "Do the segmentation process and return the resulting L2 segments. d
  is the staffspace height and n is the staffline height."
  [in d n debug]
  (let [img (ImageIO/read (File. in))
        l0 (level0-segments img debug)
        ;; L2 segments which contains notes
        l2-notes (apply concat
                        (map #(create-level2-segments img % d n)
                             (apply concat
                                    (map #(create-level1-segments
                                           img % (detect-notes img% d n))
                                         l0))))
        ;; The other L2 segments
        l2-symbols (apply concat
                          (map #(create-level2-segments img % d n)
                               (filter #(not (has-note img % d n)) l0)))]
    (when debug
      (color-segments img l2-notes
                      :outfile "/tmp/l2-segments-notes-debug.png"
                      :color 0xFFFFFF
                      :black 0xFF0000
                      :other-black 0x00FF00)
      (color-segments img l2-symbols
                      :outfile "/tmp/l2-segments-notes-symbols.png"
                      :color 0xFFFFFF
                      :black 0xFF0000
                      :other-black 0x00FF00))
    (concat l2-notes l2-symbols)))

(defn segmentation
  "Do the segmentation process, writing all the segments found in
  out-segments as a list of 4 elements vectors ([start-x start-y end-x
  end-y])"
  [in-img in-refs out-segments]
  (let [[d n] (read-vector in-refs)
        segments (find-segments in-img d n false)
        segments-vectors (map (fn [seg] [(:start-x seg)
                                         (:start-y seg)
                                         (:end-x seg)
                                         (:end-y seg)])
                              segments)]
    (write-vector out-segments segments-vectors)))