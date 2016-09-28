import sys
import os
import time
from time import strftime
import datetime
from lxml import etree
import unicodecsv
#from StringIO import StringIO

# Define the operating system dependent paths
from sys import platform as operating_system

file_to_parse = "2015-08-15_til_2015-08-01.txt"

if operating_system == "darwin":
    # OS X
    elTree = etree.parse("/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/" + file_to_parse, parser)
    parsed_file = "/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/" + "parsed_" + file_to_parse)
    lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
    output_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + search_words + '/' + start_date + '_til_' + end_date + '.txt'
elif operating_system == "win32":
# Windows...
    elTree = etree.parse("C:\\Users\\dev231\\Box Sync\\scrape_raw\\" + file_to_parse, parser)
    parsed_file = "C:\\Users\\dev231\\Box Sync\\scrape_raw\\" + "parsed_" + file_to_parse
    lite_profile = webdriver.FirefoxProfile('C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox\\')
    output_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + search_words + '\\' + start_date + '_til_' + end_date + '.txt'
elif operating_system == "linux" or operating_system == "linux2":
    # linux
    print("We aren't set up for a Linux system...")
    
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

parser = etree.HTMLParser(remove_blank_text=True)

#elTree = etree.parse("/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/2015-08-31_til_2015-05-31.txt", parser)   # in case we need it here again

timestamps = elTree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
usersName = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')

#tweetsID = elTree.xpath('//a[@class="js-stream-item stream-item stream-item expanding-stream-item"]')

usersID = elTree.xpath('//a[@class="account-group js-account-group js-action-profile js-user-profile-link js-nav"]')

texts = elTree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')

retweets = elTree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')

favs = elTree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')

#################################################
# robo
#texts = elTree.xpath('//p[@class="TweetTextSize js-tweet-text tweet-text"]')
#################################################

timestamps = [x.get('title') for x in timestamps]
usersName = [x.get('href') for x in usersName]
#tweetsID = [x.get('data-item-id') for x in tweetsID]
usersID = [x.get('data-user-id') for x in usersID]
texts = [remove_new_line(x.xpath("string()")) for x in texts]
retweets = [x.text for x in retweets]
favs = [x.text for x in favs]

if(not len(timestamps)==len(usersName)==len(usersID)==len(texts)==len(retweets)==len(favs)):
	print('Length of parsed HTML data not equal: timestamps = %i, texts = %i, retweets = %i, favs = %i, users = $i' % (len(timestamps), len(texts), len(retweets), len(favs), len(users)))


#parsed_file = "/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/2014-06-01_til_2014-05-30.txt-parsed.txt"
write_csv(parsed_file, ['time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'], [timestamps, retweets, favs, usersName, usersID, texts])
