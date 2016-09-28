import os
import re
from sys import platform as operating_system
import time
from time import strftime
from lxml import etree
import unicodecsv
import fnmatch


## ==================================== ##
##  Create mini functions to use later  ##
## ==================================== ##

def remove_new_line(data):
    if isinstance(data, basestring):
        return ' '.join(data.splitlines())
    else:
        return data

def write_csv(fPath, header, data):
    with open(fPath, 'wb') as csvfile:
        file_writer = unicodecsv.writer(csvfile, delimiter='\t', encoding = 'utf-8')
        file_writer.writerow(header)
        file_writer.writerows(zip(*data))

## ========================== ##
##  Define the element trees  ##
## ========================== ##
        
# Define two functions that will be required
def my_parser(elTree):
    tweetID = elTree.xpath("//div[starts-with(@class, 'tweet original-tweet js-original-tweet js-stream-tweet js-actionable-tweet js-profile-popup-actionable')]")
    timestamps = elTree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
    retweets = elTree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
    favs = elTree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
    usersName = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
    usersID = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
    texts = elTree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')  # The double-space in the class name NEEDS to be there

    # Attempt to obtain clean output 
    try:
        tweetID = [re.match(r".*?(\d+)", x) for x in tweetID]
        timestamps = [x.get('title')[8:] for x in timestamps]
        retweets = [re.match(r".*?([0-9]+)", x.text).group(1) for x in retweets]
        favs = [re.match(r".*?([0-9]+)", x.text).group(1) for x in favs]
        usersName = [x.get('href')[2:] for x in usersName]
        usersID = [x.get('data-user-id') for x in usersID]
        texts = [remove_new_line(x.xpath("string()")) for x in texts]

    # Take the dirtier version of the date column if a __typeError__ is thrown
    except TypeError:
        print "There was a TypeError in file: " + each_file

        tweetID = [x.get('data-item-id') for x in tweetID]
        timestamps = [x.get('title') for x in timestamps]
        retweets = [re.match(r".*?([0-9]+)", x.text).group(1) for x in retweets]
        favs = [re.match(r".*?([0-9]+)", x.text).group(1) for x in favs]
        usersName = [x.get('href') for x in usersName]
        usersID = [x.get('data-user-id') for x in usersID]
        texts = [remove_new_line(x.xpath("string()")) for x in texts]

    if(not len(tweetID)==len(timestamps)==len(retweets)==len(favs)==len(usersName)==len(usersID)==len(texts)):
	print('Length of parsed HTML data not equal: tweetID = %i, timestamps = %i, retweets = %i, favs = %i, usersName = %i, usersID = %i, texts = %i' % (len(tweetID), len(timestamps), len(retweets), len(favs), len(usersName), len(usersID), len(texts)))
            
    return  tweetID, timestamps, retweets, favs, usersName, usersID, texts

## ====================== ##
##  Complete the parsing  ##
## ====================== ##

print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

if operating_system == "darwin":
    sep = "/"

    input_path = '/Volumes/Mac OS Drive/Thesis/Data/scrape_raw/'
    output_path = '/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/dirty/'
    
elif operating_system == "win32":
    sep = "\\"
    
    input_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\scrape_raw\\"
    output_path = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\dirty\\"

dir_list = [x[0] for x in os.walk(input_path)][1:] ## returns parent & immediate subdirctories, hence [1:]

start_time = time.time()
for each_folder in dir_list:                                ## each_folder contains the complete path
    
    file_list = list(os.walk(each_folder))[0][2]        ## each_file is simply the name of the file
    file_list = fnmatch.filter(file_list, "*.txt")      ## filters out any system files, e.g. ".DS_Store" on Darwin
        
    for each_file in file_list:
        curr_file =  each_folder + sep + each_file
        #if not os.path.isfile(each_file):
        parser = etree.HTMLParser(remove_blank_text=True, remove_comments=True)
        elTree = etree.parse(curr_file, parser)

        tweetID, timestamps, retweets, favs, usersName, usersID, texts = my_parser(elTree)

        curr_folder = os.path.split(each_folder)[1]
        output_folder = output_path + curr_folder + sep
        parsed_file = output_folder + each_file

        # Create the directory for the result if necessary, then save the output file there...
        if not os.path.exists(os.path.dirname(parsed_file)):
            os.makedirs(os.path.dirname(parsed_file))

        with  open(parsed_file, "w") as file:
            write_csv(parsed_file, ['tweetID', 'time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'],
                      [tweetID, timestamps, retweets, favs, usersName, usersID, texts])
        time.sleep(1)

time_taken = time.time() - start_time
mins, secs = divmod(time_taken, 60)
print("+ --------------------------------------------------- +")
print("+ Parsing successfuly completed for folder: " + each_folder)
print("+ Time taken (MM:SS) : " + "%02d:%02d" % (mins, secs) + " \n")
print("+ --------------------------------------------------- +\n")
