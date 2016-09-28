import os
import re
from sys import platform as operating_system
import time
from time import strftime
import datetime
from lxml import etree
import unicodecsv
#from StringIO import StringIO




###############  Commented out for testing
# # The folder name comes from the words that were searched for originally
# folder = "federal reserve"

# # Set the path to the scrape output files
# if operating_system == "darwin":
#     input_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + folder + '/'
# elif operating_system == "win32":
#     input_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + folder + '\\'


# for fn in os.listdir('.'):
#      if os.path.isfile(fn):
#         print (fn)

# file_to_parse = "2015-08-15_til_2015-08-01.txt"
################

# Define the operating system dependent paths

# File name of a raw scrape output file

parser = etree.HTMLParser(remove_blank_text=True)
elTree = etree.parse("/Volumes/FAT/Data/scrape_raw/Dow Jones/2014-05-02_til_2014-04-01.txt", parser)
# if operating_system == "darwin":
#     # OS X
#     elTree = etree.parse("/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/Merkel Deutschland/" + file_to_parse, parser)
#     parsed_file = "/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_parsed/" + file_to_parse
# #    lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
# #    output_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + search_words + '/' + start_date + '_til_' + end_date + '.txt'
# elif operating_system == "win32":
# # Windows...
#     elTree = etree.parse("C:\\Users\\dev231\\Box Sync\\scrape_raw\\federal reserve\\" + file_to_parse, parser)
#     parsed_file = "C:\\Users\\dev231\\Box Sync\\scrape_parsed\\" + file_to_parse
# #    lite_profile = webdriver.FirefoxProfile('C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox\\')
# #    output_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + search_words + '\\' + start_date + '_til_' + end_date + '.txt'
# elif operating_system == "linux" or operating_system == "linux2":
#     # linux
#     print("We aren't set up for a Linux system...")
# else:
#     print("The operating system is not recognised by the Python compiler")
    
print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

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

#elTree = etree.parse("/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/2015-08-31_til_2015-05-31.txt", parser)   # in case we need it here again

timestamps = elTree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
usersName = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
usersID = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')
texts = elTree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')
retweets = elTree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
favs = elTree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
#tweetsID = elTree.xpath('/html/body/div[2]/div[2]/div/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/ol[1]')
#tweetsID = elTree.xpath('//a[@class="js-stream-item stream-item stream-item expanding-stream-item"]')

#################################################
# robo
#texts = elTree.xpath('//p[@class="TweetTextSize js-tweet-text tweet-text"]')
#################################################

timestamps = [x.get('title')[8:] for x in timestamps]
retweets = [re.match(r".*?([0-9]+)", x.text).group(1) for x in retweets]
favs = [re.match(r".*?([0-9]+)", x.text).group(1) for x in favs]
usersName = [x.get('href')[2:] for x in usersName]
usersID = [x.get('data-user-id') for x in usersID]
texts = [remove_new_line(x.xpath("string()")) for x in texts]
#tweetsID = [re.match(r"\d*$", x.text).group(1) for x in tweetsID]

#tweetsID = [x.get('data-item-id') for x in tweetsID]

if(not len(timestamps)==len(usersName)==len(usersID)==len(texts)==len(retweets)==len(favs)):
	print('Length of parsed HTML data not equal: timestamps = %i, texts = %i, retweets = %i, favs = %i, users = %i, tweetsID = %i' % (len(timestamps), len(texts), len(retweets), len(favs), len(usersID), ))


# # Create the directory for the result if necessary, then save the output file there...
# if not os.path.exists(os.path.dirname(output_file)):
#     os.makedirs(os.path.dirname(output_file))
# with  open(output_file, "w") as file:
#       file.write(driver.page_source.encode('utf-8'))

parsed_file = '/Users/nicholasmitchell/Volumes/2014-05-02_til_2014-04-01.txt'
write_csv(parsed_file, ['time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'], [timestamps, retweets, favs, usersName, usersID, texts])
