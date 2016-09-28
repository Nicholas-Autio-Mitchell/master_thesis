import os
import re
from sys import platform as operating_system
import time
from time import strftime
from lxml import etree
import unicodecsv
#import datetime
#import lxml
#from StringIO import StringIO


# The folder name comes from the words that were searched for originally
folder = "federal reserve"

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

# Define two functions that will be required
def my_parser(elTree):

    timestamps = elTree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
    usersName = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
    usersID = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
    texts = elTree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')
    retweets = elTree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
    favs = elTree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
    #tweetsID = elTree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip]')

    # Full xPath from Firebug
        #/html/body/div[2]/div[2]/div/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/ol[1]/li[7]/div/div[2]/div[1]/small/a
    # The HTML Code for a tweet-id
        #<a class="tweet-timestamp js-permalink js-nav js-tooltip" href="/aDFWTaxiCab/status/643569645308497920" data-original-title="4:38 PM - 14 Sep 2015">


    # Attempt to obtain clean output 
    try:
        timestamps = [x.get('title')[8:] for x in timestamps]
        retweets = [re.match(r".*?([0-9]+)", x.text).group(1) for x in retweets]
        favs = [re.match(r".*?([0-9]+)", x.text).group(1) for x in favs]
        usersName = [x.get('href')[2:] for x in usersName]
        usersID = [x.get('data-user-id') for x in usersID]
        texts = [remove_new_line(x.xpath("string()")) for x in texts]
        #tweetsID = [re.match(r"\d*$", x.text).group(1) for x in tweetsID]

    # Take the dirtier ersion is the __typeError__ is thrown
    except TypeError:
        print "There was a TypeError and so the dates column won't be very tidy"
        timestamps = [x.get('title') for x in timestamps]
        retweets = [re.match(r".*?([0-9]+)", x.text).group(1) for x in retweets]
        favs = [re.match(r".*?([0-9]+)", x.text).group(1) for x in favs]
        usersName = [x.get('href') for x in usersName]
        usersID = [x.get('data-user-id') for x in usersID]
        texts = [remove_new_line(x.xpath("string()")) for x in texts]
        #tweetsID = [re.match(r"\d*$", x.text).group(1) for x in tweetsID]
        
        
    if(not len(timestamps)==len(retweets)==len(favs)==len(usersName)==len(usersID)==len(texts)):
            print('Length of parsed HTML data not equal:'
            'timestamps = %i, usersName = %i, usersID = i%, texts = %i, retweets = %i, favs = %i'
            % len(timestamps)==len(retweets)==len(favs)==len(usersName)==len(usersID)==len(texts))

    return timestamps, retweets, favs, usersName, usersID, texts

print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

# Carry out the parsing depending on operating_system
if operating_system == "darwin":
    # Set the path to the scrape output files
    start_time = time.time()
    input_folder = '/Volumes/Mac OS Drive/Data Backup/Data/scrape_raw/' + folder + '/'
    output_folder = '/Volumes/Mac OS Drive/Data Backup/Data/parse_output/individual/' + folder + '/'
    for each_file in os.listdir(input_folder): ##  Add a filter here?   if str(each_file)[ ==
        if not os.path.isfile(each_file):
            parser = etree.HTMLParser(remove_blank_text=True, remove_comments=True)
            elTree = etree.parse(input_folder + each_file, parser)

            timestamps, retweets, favs, usersName, usersID, texts = my_parser(elTree)

            parsed_file = output_folder + each_file

            # Create the directory for the result if necessary, then save the output file there...
            if not os.path.exists(os.path.dirname(output_folder)):
                os.makedirs(os.path.dirname(output_folder))

            with  open(parsed_file, "w") as file:
                  write_csv(parsed_file, ['time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'],
                            [timestamps, retweets, favs, usersName, usersID, texts])
            time.sleep(1)

    time_taken = time.time() - start_time
    mins, secs = divmod(time_taken, 60)
    print("+ --------------------------------------------------- +")
    print("+ Parsing successfuly completed for folder: " + folder)
    print("+ Time taken (MM:SS) : " + "%02d:%02d" % (mins, secs) + " \n")
    print("+ --------------------------------------------------- +\n")


            
# elif operating_system == "win32":
#     # Set the path to the scrape output files
#     input_folder = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + folder + '\\'
#     output_folder = 'C:\\Users\\dev231\\Box Sync\\scrape_parsed\\' + folder + '\\'
#     for each_file in os.listdir(input_folder):
# #         if os.path.isfile(each_file):
#         elTree = etree.parse(input_folder + each_file, parser)
#         parsed_file = output_folder + each_file

#         timestamps, retweets, favs, usersName, usersID, texts = parse(elTree)

#         write_csv(parsed_file, ['time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'], [timestamps, retweets, favs, usersName, usersID, texts])

# else:
#     print("The operating system is not recognised by the Python compiler")



#elTree = etree.parse("/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/2015-08-31_til_2015-05-31.txt", parser)   # in case we need it here again


