#import os
# from subprocess import call


# ## Create string to send to terminal
# input_path = "/Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/dow_SPDR/"

# curr_file = "2013-02-12_til_2013-01-28_clean_text.txt"

# curr_input = input_path + curr_file

# shell_command = "java -jar /Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/SentiStrength.jar sentidata /Volumes/Mac\ OS\ Drive/Thesis/Sentiment_Analysis/sentimental-analysis-methods/sentistrength/ input " + curr_input + " > /Volumes/Mac\ OS\ Drive/Thesis/Data/sentiment_analysis/output/SentiStrength/" + curr_file

# call(shell_command, shell=True)


import os

for root, dirs, files in os.walk('/Volumes/Mac OS Drive/Thesis/Data/input/clean/'):
    print root, "\n"  # I have no actual need to print these, it is just to help me
    print dirs, "\n"
    [analysis_function(name) for name in files]
