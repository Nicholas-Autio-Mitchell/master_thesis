
## ================================================================================= ##
##  Extract text from parsing output to clean in R and pass into sentiment analysis  ##
## ================================================================================= ##

import os
from sys import platform as operating_system
import csv
import time

# Carry out tasks dependant on operating_system
if operating_system == "darwin":
    sep = "/"

    input_path = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/dirty/"

    output_path = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/text_only/"

elif operating_system == "win32":
    sep = "\\"
    
    input_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\dirty\\"

    output_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\text_only\\"

def text_extractor(input_path, output_path, sep):

    dir_list = [x[0] for x in os.walk(input_path)][1:] ## returns parent & immediate subdirctories, hence [1:]

    start_time = time.time()

    for each_folder in dir_list:

        file_list = list(os.walk(each_folder))[0][2]      ## each_file is simply the name of the file

        for each_file in file_list:

            curr_file = each_folder + sep + each_file

            curr_folder = os.path.split(each_folder)[1]

            output_folder = output_path + curr_folder + sep

            output_file = output_folder + each_file + "_dirty_text.txt"

            # Create the directory for the result if necessary, then save the output file there...
            if not os.path.exists(os.path.dirname(output_folder)):
                os.makedirs(os.path.dirname(output_folder))

            with open(curr_file, "r") as to_read:
                with open(output_file, "wb") as to_write:
                    reader = csv.reader(to_read, delimiter = "\t")
                    writer = csv.writer(to_write)

                    desired_column = [6]        # text column

                    for row in reader:
                        myColumn = list(row[i] for i in desired_column)    
                        writer.writerow(myColumn)

            ##time.sleep(1)       # Can help things run smoothly

    time_taken = time.time() - start_time
    mins, secs = divmod(time_taken, 60)

    print("+ --------------------------------------------------- +")
    print("+ Text extraction successful")
    print("+ Files containing only tweet-text ready for cleaning")
    print("+ Time taken (MM:SS) : " + "%02d:%02d" % (mins, secs))
    print("+ --------------------------------------------------- +\n")      
