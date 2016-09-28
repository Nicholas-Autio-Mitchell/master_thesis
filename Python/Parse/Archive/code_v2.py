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

import xlsxwriter as xw

from lxml import etree
from lxml.html.soupparser import fromstring
from lxml.etree import ElementTree
from lxml.html.clean import Cleaner



STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2


class Sel(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = " https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2015-01-01%20until%3A2015-08-26&src=typd"
        self.verificationnErrors = []
        self.accept_next_alert = True
    def test_sel(self):
        driver = self.driver
        delay = 3
        driver.get(self.base_url)
        for i in range(1,3):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(3)
        
        myFile = open('/Volumes/SSDizzle/test.txt','w')
        myFile.write(driver.page_source.encode('utf-8'))
        myFile.close()
        
        # get all the details via lxml
        parser = etree.HTMLParser(remove_blank_text=True)
        tree = etree.parse("'/Volumes/SSDizzle/test.txt",parser)
        timestamps = tree.xpath('//a[@class="tweet-timestamp js-permalink js-nav js-tooltip"]')
        texts=tree.xpath('//p[@class="TweetTextSize  js-tweet-text tweet-text"]')
        retweets=tree.xpath('//span[@class="ProfileTweet-action--retweet u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
        favs=tree.xpath('//span[@class="ProfileTweet-action--favorite u-hiddenVisually"]/span/span[@class="ProfileTweet-actionCountForAria"]')
        
        # write to excel file
        workbook = xw.Workbook('/Volumes/SSDizzle/test.txt/output.xlsx')
        worksheet = workbook.add_worksheet('tweeter data')
        
        worksheet.write_row(0, 0, ['Time / date','Retweets','Favorites','Text'])
        for i in range(len(timestamps)):
            worksheet.write_row(i+1, 0, [timestamps[i].get('title'),retweets[i].text,favs[i].text,texts[i].xpath("string()")])
        
        workbook.close()

if __name__ == '__main__':
    
    try:
        unittest.main()

    except (SystemExit, KeyboardInterrupt):
        exit_status = STATUS_OK
    except:
        logging.error('Abnormal program termination: %s' % sys.exc_info()[1])
        import traceback
        traceback.print_exc()
        exit_status = STATUS_FAILURE
    finally:
        logging.info('Finished with status: %s' % exit_status)
        
