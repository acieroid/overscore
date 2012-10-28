INPUT="$1"
OUTPUT="$2"

AUDIVERIS_PATH=/opt/audiveris
AUDIVERIS="java -jar $AUDIVERIS_PATH/dist/audiveris-4.1beta.jar"

echo "OMRing $INPUT with audiveris, exporting to $OUTPUT"
$AUDIVERIS -batch -step SCORE -input "$INPUT" -export "$OUTPUT"
