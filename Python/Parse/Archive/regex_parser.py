## ==================================================== ##
##  This file can extract a matched search using regex  ##
## ==================================================== ##

import re

filename = '/Volumes/Mac OS Drive/Data Backup/Data/scrape_raw/market volatility/2013-01-28_til_2013-01-13.txt'
pattern = "\d{18}"

#pattern  = 'id=\"stream-item-tweet-\d+'

new_file = []

# Make sure file gets closed after being iterated
with open(filename, 'r') as f:
   # Read the file contents and generate a list with each line
   lines = f.readlines()

# Iterate each line
for line in lines:

    # Regex applied to each line 
    match = re.search(pattern, line)
    
    # match_str = [re.match(r"$\d+", x.text) for x in match]
    if match:
        # match_str = match.group()
        # tweet_id = re.findall(r"\d+", match_str) + "\n"
        # Make sure to add \n to display correctly when we write it back
        #new_line = tweet_id + "\n"
        new_line = match.group() + '\n'
        print new_line
        new_file.append(new_line)

with open(filename + "tweetsID.txt", 'w') as f:
     # go to start of file
     f.seek(0)
     # actually write the lines
     f.writelines(new_file)
