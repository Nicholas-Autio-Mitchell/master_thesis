__author__ = 'nicholasmitchell'

import logging

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import sys


import unittest, time, re

# import xlsxwriter as xw

# from lxml import etree
# from lxml.html.soupparser import fromstring
# from lxml.etree import ElementTree
# from lxml.html.clean import Cleaner

# sys.path.append('C:\Users\dev231\Dropbox\Thesis\')

STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2

# Loop through the months and save months of tweets separately
#month = ["12", "11", "10", "09", "08", "07", "06", "05", "04", "03", "02", "01"]
#days_in_month = ["31", "30", "31", "30", "31", "31", "30", "31", "30", "31", "28", "31"]
#number_of_scrolls = 0

#for i in range(0,11):
 #   #print(until_month[i])
  #  url = "https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2014-" + month[i+1] + "-" + days_in_month[i+1] + "%20until%3A2014-" + month[i] + "-" + days_in_month[i] + "&src=typd"


#lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
#browser = webdriver.Firefox(lite_profile)
#"/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite"

class Sel(unittest.TestCase):
    def setUp(self):
        lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
        self.driver = webdriver.Firefox(lite_profile)
        #self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        #self.base_url = url
        self.base_url = "https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2015-08-28%20until%3A2015-08-30&src=typd"
        self.verificationnErrors = []
        self.accept_next_alert = True
    def test_sel(self):
        driver = self.driver
        delay = 3
        driver.get(self.base_url)
        for j in range(1,30):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(1)

        myFile = open("/Volumes/Mac OS Drive/Documents/Dropbox/Thesis/PyCharm_Projects/raw_output/2015-08-28_til_2015-08-30.txt", 'w')
        myFile.write(driver.page_source)
        #.encode('utf-8'))
        myFile.close()

# class Sel(unittest.TestCase):
#     def setUp(self):
#         lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
#         self.driver = webdriver.Firefox(lite_profile)
#         #self.driver = webdriver.Firefox()
#         self.driver.implicitly_wait(30)
#         #self.base_url = url
#         self.base_url = "https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2014-10-301%20until%3A2014-08-31&src=typd"
#         self.verificationnErrors = []
#         self.accept_next_alert = True
#     def test_sel(self):
#         driver = self.driver
#         delay = 3
#         driver.get(self.base_url)
#         for j in range(1,800):
#             self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
#             number_of_scrolls = 0
#             number_of_scrolls = number_of_scrolls + j
#             time.sleep(2)
#         print(number_of_scrolls)
#
#         myFile = open("/Volumes/Mac OS Drive/Documents/Dropbox/Thesis/PyCharm_Projects/test1/2015-02-28_til_2015-01-01.txt", 'w')
#         myFile.write(driver.page_source)
#         #.encode('utf-8'))
#         myFile.close()



        # # get all the details via lxml
        # parser = etree.HTMLParser(remove_blank_text=True)
        # tree = etree.parse("C:\\Users\\dev231\\Dropbox\\Thesis\\test.txt",parser)
        # timestamps = tree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
        # texts=tree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')
        # retweets=tree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
        # favs=tree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')

        # # write to excel file
        # workbook = xw.Workbook('C:\\Users\\dev231\\Dropbox\\Thesis\\output.xlsx')
        # worksheet = workbook.add_worksheet('tweeter data')

        # worksheet.write_row(0, 0, ['Time / date','Retweets','Favorites','Text'])
        # for i in range(len(timestamps)):
            # worksheet.write_row(i+1, 0, [timestamps[i].get('title'),retweets[i].text,favs[i].text,texts[i].xpath("string()")])

        # workbook.close()

# if __name__ == '__main__':

    # try:
        # unittest.main()

    # except (SystemExit, KeyboardInterrupt):
        # exit_status = STATUS_OK
    # except:
        # logging.error('Abnormal program termination: %s' % sys.exc_info()[1])
        # import traceback
        # traceback.print_exc()
        # exit_status = STATUS_FAILURE
    # finally:
        # logging.info('Finished with status: %s' % exit_status)

