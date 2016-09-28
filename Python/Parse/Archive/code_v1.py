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

STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2


class Sel(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = " https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2015-06-30%20until%3A2015-07-15&src=typd"
        self.verificationnErrors = []
        self.accept_next_alert = True
    def test_sel(self):
        driver = self.driver
        delay = 3
        driver.get(self.base_url)
        for i in range(1,501):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(3)
            favs=driver.find_elements_by_class_name('ProfileTweet-actionCountForAria')
            
        html_source = driver.page_source
        
        texts=driver.find_elements_by_class_name('TweetTextSize')
        
        # >\d{0,10} retweet
        # >\d{1,10} favorite
        # tweet-timestamp js-permalink js-nav js-tooltip
        
        myFile = open('C:\\Work\\DEVnet\\Projects\\Scroll\\test.txt','w')
        myFile.write(driver.page_source.encode('utf-8'))
        myFile.close()
        
#         data.txt = html_source.encode('utf-8')

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
        
