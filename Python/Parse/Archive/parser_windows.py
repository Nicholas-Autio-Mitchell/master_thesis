import sys
import os

from lxml import etree
import unicodecsv
#from StringIO import StringIO


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
elTree = etree.parse("C:\Users\dev231\Dropbox\Thesis\PyCharm_Projects\raw_output\2014-10-30_til_2014-08-01.txt", parser)

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


tmp_file = "C:\Users\dev231\Dropbox\Thesis\PyCharm_Projects\raw_output\parsed\2014-10-30_til_2014-08-01.txt-parsed_with_user_test.txt"
write_csv(tmp_file, ['time_date', 'times_retweeted', 'times_favourited', 'user_name', 'user_id', 'text'], [timestamps, retweets, favs, usersName, usersID, texts])
