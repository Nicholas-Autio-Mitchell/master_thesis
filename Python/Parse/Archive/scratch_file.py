import os
#import sys

input_path = '/Volumes/Mac OS Drive/Thesis/Data/scrape_raw/'
t1 = os.listdir(input_path)
t2 = os.walk(input_path)
for fname in os.listdir(input_path):
    path = os.path.join(input_path, fname)
    if os.path.isdir(path):
        # skip directories
        continue

## ============================================================== ##
##  Loop through each search term folder and its files to scrape  ##
## ============================================================== ##

input_path = '/Volumes/Mac OS Drive/Thesis/Data/scrape_raw/'

dir_list = [x[0] for x in os.walk(input_path)][1:] ## returns parent dir plus all immediate subdirctories



for each_folder in dir_list:
        if not os.path.isfile(curr_file):
            for fname in os.listdir(input_path):
                path = os.path.join(input_path, fname)
                if os.path.isdir(path):
                    # skip directories
                    continue
#
input_path = '/Volumes/Mac OS Drive/Thesis/Data/scrape_raw/bear market/2013-01-28_til_2013-01-13.txt'
if not os.path.isfile(input_path):
    print "the file is there"
else:
    print "the file isn't there"
    
#files = [f for f in os.listdir(dirToScreens) if path.isfile(path.join(dirToScreens, f))]

subdirectories = os.listdir(input_path)
os.path.isdir






files_to_parse = os.path.isfile(os.path.join(your_directory, f))

[f for f in os.listdir(your_directory) if os.path.isfile(os.path.join(your_directory, f))]





input_path = '/Volumes/Mac OS Drive/Data Backup/Data/scrape_raw/'

import os
filter(os.path.isfile, os.listdir( ))
