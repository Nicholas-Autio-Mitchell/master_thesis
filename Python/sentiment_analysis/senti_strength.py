
## ================================================================== ##
##  Evaluate each text file individually for the SentiStrength model  ##
## ================================================================== ##

from subprocess import call
from sys import platform as operating_system
from file_mover import gen_find



## Create the command to send to terminal
if operating_system == "darwin":
    
    sep = "/"
    model = "/Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/"
    models = "/Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/"
    java_jar = "java -jar " + model + "SentiStrength.jar"
    sentidata = " sentidata" + model

    input_path = "/Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/input/clean/" + curr_folder

    ## ===================================================================================================== ##
    ##  There is no output folder because the analysis is created in the working directory of the .jar file
    ##  The results are then programatically copied to our output folder from which R reads them.
    ## ===================================================================================================== ##


elif operating_system == "win32":

    sep = "\\"
    java_jar = "java -jar C:\\Users\\dev231\\Documents\\Thesis\\Sentiment_Analysis\\sentiment-analysis-methods\\sentistrength\\SentiStrength.jar"

    sentidata = " sentidata /Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/"

    input_path = "/Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/dow_SPDR/"


    curr_file = "2013-01-28_til_2013-01-13_clean_text.txt"

    curr_input = input_path + curr_file





    
shell_command = java_jar + sentidata + " inputFolder "  + input_path + " overwrite"

call(shell_command, shell=True)


call( v_afinn.py -f example.txt",shell=True))

