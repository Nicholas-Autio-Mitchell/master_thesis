__author__ = 'matheus'
from vaderSentiment import vaderSentiment
from optparse import OptionParser


def checkText(text):
    analyse = vaderSentiment.sentiment(text)
    compound = analyse["compound"]
    #print compound
    
    if compound < -0.5:
        return str(compound) + "\t" + str(-1)
    elif compound > 0.5:
        return str(compound) + "\t" + str(1)
    else:
        return str(compound) + "\t" + str(0)

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-f', '--filename', dest='filename')
    parser.add_option('-t', '--text', dest='text')
    options, args = parser.parse_args()

    if options.text:
        print checkText(options.text)

    elif options.filename:
        with open(options.filename) as F:
            for text in F:
                print checkText(text)
