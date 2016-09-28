
import csv

# File names in use
input_file = "tester_2014-10-30_til_2014-08-01.txt"
output_file = input_file + "_SA_input.txt"

## ========== ## 
##  SO help2  ##  --> this works correctly
## ========== ##  --> http://stackoverflow.com/questions/33217277/read-and-export-a-single-column-from-a-tab-separated-file-in-python

with open(input_file) as to_read:
    with open(output_file, "wb") as tmp_file:
        reader = csv.reader(to_read, delimiter = "\t")
        writer = csv.writer(tmp_file)

        desired_column = [6]        # text column

        for row in reader:
            myColumn = list(row[i] for i in desired_column)    
            writer.writerow(myColumn)


## ===================== ##
##  My attempt using csv ##
## ===================== ##
# with open(input_file) as to_read:
#     reader = csv.reader(to_read, delimiter = "\t")

#     # for row in reader:
#     #     print row
#     # --> This shows that the whole tsv file is being read in correctly to this point
    
#     #column_titles = ["tweetsID", "tweet_date", "times_retweeted", "times_favourited", "user_name", "user_id", "text"]
#     desired_column = 6        # text column

#     for row in reader:
#         myColumn = list(row[desired_column])

# with open(output_file, "wb") as tmp_file:
#     writer = csv.writer(tmp_file)
    
#     for row in myColumn:
#         writer.writerow(row)

## ========= ##
##  SO help1 ##
## ========= ##
     
# text_strings = [] # empty array to store the last column text

# with open(input_file) as ff:
#     ss = ff.readlines() # read all strings in a string array 

# for s in ss:
#     text_strings.append(s.split('\t')[-1]) # last column to the text array

# with open(output_file) as outf:
#     ff.write('\n'.join(text_strings)) # write everything to output file
        
## ======================== ##
##  Using module 'pandas'  ##
## ======================== ##
# Doesn't work on my machine - incorrect installation of pandas?

# import pandas as bamboo
# with open(input_file) as to_read:
#     myDataFrame = bamboo.read_csv(to_read)
#     myOutput = myDataFrame.text
