(ns overscore.main
  (:gen-class)
  (:use overscore.musicxml
        overscore.generator
        overscore.preprocessing.preprocessing
        overscore.staffline.staffline))

(defn usage []
  (println
   "Possible arguments:\n"
   "\tpreprocessing <in> <out> <out-ref>\n\t\tPreprocess the image <in>, saving the output image to <out> and the reference lengths descriptions in <out-ref>\n"
   "\tstaffline <in>\n\t\tIsolate the systems and find the staffline positions on each system, from the image <in>. Saves the output for each system to <in>-n.png and <in>-n.txt, where n is an integer\n"
   "\tgenerate <in> <out> <name>: parse the <in> MusicXML file, and generate the song <name> in the clojure file <out>\n"
   "\tplay <file> <name>: play the song called <name> defined in the file <file>\n"))

(defn call-if
  "Apply the last argument to the first one if its length is equal to
  n. Else, print the usage of this program."
  [args n f]
  (if (= (count args) n)
    (apply f args)
    (usage)))

(defn generate [in out name]
  (write-to-file
   (parse-musicxml in) (symbol name) out)
  (println "Generated song" name "in file" out))

(defn play-song [file name]
  (println "Not implemented yet"))

(defn -main [& args]
  (case (first args)
    "generate"
    (call-if (rest args) 3 generate)
    "play"
    (call-if (rest args) 2 play-song)
    "preprocessing"
    (call-if (rest args) 3 preprocessing)
    "staffline"
    (call-if (rest args) 1 staffline-processing)
    (usage)))
