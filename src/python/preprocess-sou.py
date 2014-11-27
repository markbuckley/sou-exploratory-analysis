import sys
import base64
import csv

"""
Reads the text file of State of the Union speeches from here:
http://stateoftheunion.onetwothree.net/texts/stateoftheunion1790-2014.txt.zip

and creates a CSV file of the form

  title, name, date, text

where the text is base64 encoded.
"""


def readSouSpeeches(filename):
  """
  returns a list of speeches, 
  each is a dictionary of title, name, date, text
  """
  with open(filename,'r') as f:
    data = f.read().split('\n')

  line = 0
  speeches = []
  speechCount = 0

  while line < len(data):# and speechCount < 2:

    if data[line] == "***":
      speechCount += 1

      line += 2
      title = data[line]
      line += 1
      name = data[line]
      line += 1
      date = data[line]
      line += 2

      # now the text of the speech starts
      # line is now the row number of the first line of the speech text
      endOfSpeech = line
      while endOfSpeech < len(data) and data[endOfSpeech] != "***":
        endOfSpeech += 1

      # now endOfSpeech is the line number of the last line of the speech
      text = ' '.join(data[line:endOfSpeech])

      # advance the line pointer to the end of the current speech
      line = endOfSpeech-1

      speech = {'title': title, 'name':name, 'date': date, 'text':text}

      #print speech
      speeches.append(speech)

    line += 1

  print "Speeches found: %s" % len(speeches)

  return speeches
  

def writeToFile(speeches, filename):
  """
  Writes the given list of speeches to a CSV file
  """
  with open(filename, 'wb') as csvfile:

    speechwriter = csv.writer(csvfile)

    for speech in speeches:
      row = [speech['title'].strip(),
             speech['name'].strip(),
             speech['date'].strip(),
             base64.b64encode(speech['text'])]

      speechwriter.writerow(row)
    

if __name__ == "__main__":

  if len(sys.argv) != 3:
    print("Two arguments required: input file, output file")
    sys.exit(-1)

  speeches = readSouSpeeches(sys.argv[1])

  writeToFile(speeches, sys.argv[2])

