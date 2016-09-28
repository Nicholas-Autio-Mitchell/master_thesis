
import os
import fnmatch

import shutil
from sys import platform as operating_system

def gen_find(file_pattern, source):
    for path, dirlist, filelist in os.walk(source):
        for name in fnmatch.filter(filelist, file_pattern):
            yield os.path.join(path, name)

# Example use

if __name__ == '__main__':
    lognames = gen_find("*_out.txt","www")
    for name in lognames:
        print name


## ========================================== ##
##  Vader - Afinn (from vader_based_lexicon)  ##
## ========================================== ##


"python /Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/vader_based_lexicons/v_afinn/v_afinn.py -f /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/input/clean/federal\ reserve/2014-08-15_til_2014-08-01_clean_text.txt > /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/output/vader_test.txt"

## This equates to simply:
"python v_afinn.py -f curr_file > curr_out.txt"

## ======== ##
##  Emolex  ##
## ======== ##

"./emolex.pl -f qt.txt > /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/output/emolex_test.txt"

## The .pl (Perl) file calls the lexicon which is in the emolex folder, and so the command needs to be run from that folder.

## ============== ##
##  Sentiment140  ##
## ============== ##

"java -jar sentiment140.jar -f qt.txt > /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/output/sentiment140_test.txt"

## Once again, we must be in directory of the files for execution. This one takes a little longer than the perl/Python ones. (deompliation of the jar file?)

## =============== ##
##  SentiStrength  ##
## =============== ##

"Can't get it to work through usage of the python code from Matheus (iFeel)"

