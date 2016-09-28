
## ======================================================== ##
##  Here is the brief summary of models tested in Terminal  ##
## ======================================================== ##
"""
The models here have worked for individual files using Terminal. A brief look into each of their methods shows that they are sound and offer differing approaches.
All the required files for execution are stored in the folder 'exec'. The file(s) for the actual execution must also be in this folder with the models and their data.

The script to run through the models below (for the collection of cleaned tweet files) is kept withi the 'exec' folder.
The data for each of the models is also to be kept in that folder, to reduce complications with system paths
"""
## ================= ##
##  Usage of Models  ##
## ================= ##

"""
The models below are run through the terminal using the example commands shown
"""

## ================= ##
##  Model 1: Emolex  ##
## ================= ##

""" 
The .pl (Perl) file calls the lexicon which is in the emolex folder, and so the command needs to be run from that folder.
Altered the perl source code to print both the raw sentiment output of string as well as a general [-1, 0, 1] - two tab-separated columnds
"""

#./emolex.pl -f curr_file.txt > curr_out_emolex_test.txt

## ======================= ##
##  Model 2: Sentiment140  ##
## ======================= ##

""" 
Once again, we must be in directory of the files for execution. This one takes a little longer than the perl/Python ones. (deompliation of the jar file?)
"""

#java -jar sentiment140.jar -f curr_file.txt > curr_out_sentiment140_test.txt

## ======================== ##
##  Model 3: SentiStrength  ##
## ======================== ##

""" 
The .jar file must be in the execution folder
Can't get it to work through usage of the python code from Matheus (iFeel) - sentistrength.py
Have to use the shell commands, which create the file where the .jar file is kept, then move the output files afterwards using file_mover.py
"""

#java -jar SentiStrength.jar sentidata data/ input curr_file

## ================================================= ##
##  Model 4: Vader-Afinn (from vader based lexicon)  ##
## ================================================= ##

""" 
This requires the v_afinn.txt file
"""

#python /Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/vader_based_lexicons/v_afinn/v_afinn.py -f /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/input/clean/federal\ reserve/2014-08-15_til_2014-08-01_clean_text.txt > curr_out_vader_test.txt

## This equates to:
#python v_afinn.py -f curr_file > curr_out.txt

## ================ ##
##  Model 5: Vader  ##
## ================ ##

""" 
I believe this uses the installed python package vaderSentiment and so requires no additional data
I altered the script to maintain the decimal scores (between 1 and -1) as well as print out 1, 0 or -1 - so the output file has tab-separated two colums
"""

#python vader.py -f curr_file.txt > curr_out_/vader_compound_test.txt


