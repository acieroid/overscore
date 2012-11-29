# Script that downloads and convert the test data to the format we want
# (300dpi PNG without alpha)
# Call this script as: awk -f download.awk data.txt

# The file data.txt contains the description of the data, as follows:
# DATA|name|type|url
# where:
#   - name is the name of the file we will output (eg. foo as name will
#     produce foo-*.png, one for each page)
#   - type is the type of the input file (eg. pdf)
#   - url is the location from where we need to retrieve the file
# The lines not starting with DATA| are considered as comments and ignored

BEGIN { FS = "|" }

/DATA\|/ {
  name = $2
  type = $3
  url = $4
  infile = name "." type
  outfile = name ".png"
  print("Retrieving " name " from location " url)
  if (system("test -f " infile) != 0) {
    system("wget '" url "' -O " infile)
  }
  print("Converting to PNG")
  system("convert -density 300 -alpha off " infile " " outfile)
}

END { print("Done") }
