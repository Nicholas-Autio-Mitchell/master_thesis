
## ================================================================================= ##
##  Extract text from parsing output to clean in R and pass into sentiment analysis  ##
## ================================================================================= ##

import os
from sys import platform as operating_system
import csv
import time

# The folder name comes from the words that were searched for originally
folder = "Dow Jones"

# Carry out tasks dependant on operating_system
if operating_system == "darwin":
    start_time = time.time()    # time the process for printed summary
    
    # Set the path to the parsed output files
    input_folder = '/Volumes/Mac OS Drive/Data Backup/Data/parse_output/individual/' + folder + '/'
    output_folder = '/Volumes/Mac OS Drive/Data Backup/Data/parse_output/tweet-text/' + folder + '/'
    for each_file in os.listdir(input_folder):
        if not os.path.isfile(each_file):

            to_extract = input_folder + each_file
            output_file = output_folder + each_file + "_SA_input.txt"
            
            # Create the directory for the result if necessary, then save the output file there...
            if not os.path.exists(os.path.dirname(output_folder)):
                os.makedirs(os.path.dirname(output_folder))

            with open(to_extract, "r") as to_read:
                with open(output_file, "wb") as tmp_file:
                    reader = csv.reader(to_read, delimiter = "\t")
                    writer = csv.writer(tmp_file)

                    desired_column = [6]        # text column

                    for row in reader:
                        myColumn = list(row[i] for i in desired_column)    
                        writer.writerow(myColumn)

            time.sleep(1)       # Helps things run smoothly

            
# File names in use
#input_file = "tester_2014-10-30_til_2014-08-01.txt"


time_taken = time.time() - start_time
mins, secs = divmod(time_taken, 60)

print("+ --------------------------------------------------- +")
print("+ Text extraction successful for folder: " + folder)
print("+ Time taken (MM:SS) : " + "%02d:%02d" % (mins, secs) + " \n")
print("+ --------------------------------------------------- +\n")
