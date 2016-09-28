
## =============================================================== ##
##  Remove all â characters that appear after removing links in R  ##
## =============================================================== ##

import os
from sys import platform as operating_system
import csv
import time
import re 

# Carry out tasks dependant on operating_system
# if operating_system == "win32":
#     start_time = time.time()    # time the process for printed summary
input_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\clean\\"
output_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\cleaner\\"

dir_list = [x[0] for x in os.walk(input_path)][1:] ## returns parent & immediate subdirctories, hence [1:]

start_time = time.time()

for each_folder in dir_list:

    file_list = list(os.walk(each_folder))[0][2]      ## each_file is simply the name of the file

    for each_file in file_list:
        
        curr_file = each_folder + "\\" + each_file
        
        curr_folder = os.path.split(each_folder)[1]
        
        output_folder = output_path + curr_folder + "\\"

        output_file = output_folder + each_file + "_no_a_hat.txt"
            
        # Create the directory for the result if necessary, then save the output file there...
        if not os.path.exists(os.path.dirname(output_file)):
            os.makedirs(os.path.dirname(output_file))

        ## Method 1 - doing a whole file at a time ##
        cleaned_file = open(curr_file).read().replace("â", "")

        with open(output_file, 'w') as to_write:
            writer = csv.writer(to_write)

            
        with open(curr_file, "r") as to_clean:
            with open(output_file, "wb") as to_write:
                reader = csv.reader(to_clean, delimiter = "\n")
                writer = csv.writer(to_write)

                for row in reader:
                    
                    cleaned = open(to_clean).read().re.sub(r"â", "")
                ## cleaned_file = re.sub(r"http\S+", "", to_clean)

                writer.writerows(cleaned)

       # for each_file in 

            
          ##  with open(output_file, "wb") as to_write:
          ##      reader = csv.reader(to_read, delimiter = "\t")
          ##      writer = csv.writer(to_write)

          ##      desired_column = [6]        # text column

          ##      for row in reader:
                    ##myColumn = list(row[i] for i in desired_column)    
                    ##writer.writerow(myColumn)

        ##time.sleep(1)       # Helps things run smoothly

            
# File names in use
#input_file = "tester_2014-10-30_til_2014-08-01.txt"


time_taken = time.time() - start_time
mins, secs = divmod(time_taken, 60)

print("+ --------------------------------------------------- +")
print("+ Cleaning completed")
print("+ Occurences of â were removed")
print("+ Time taken (MM:SS) : " + "%02d:%02d" % (mins, secs))
print("+ --------------------------------------------------- +\n")
