# Combined original code with the tutorial: http://www.marinamele.com/selenium-tutorial-web-scraping-with-selenium-and-python

import logging
import sys
import time

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException

STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2

# Create the url - assuming we are always moving backwardsd into time
# '%20' is a space in a url

#https://twitter.com/search?q=federal%20reserve%20lang%3Aen%20since%3A2015-04-30%20until%3A2015-08-31&src=typd

search_term = 'federal%20reserve'
start_date = '2015-05-31'
end_date = '2015-01-01'
url = 'https://twitter.com/search?q=' + search_term + '%20-abre%20lang%3Aen%20since%3A' + end_date + '%20until%3A' + start_date + '&src=typd'
output_windows = 'C:\\Users\\dev231\\Box Sync\\' + start_date + '_til_' + end_date + '.txt'

# Use a minimalistic firefox profile
lite_profile_windows = webdriver.FirefoxProfile('C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\5snffrxs.default\\')

print("******* Defining session *******")

class Scraper:
    def init_driver(self):
        print("******* Opening Firefox instance and navigating to page... *******")
        self.driver = webdriver.Firefox()
        print("Instance has been opened")
        self.driver.implicitly_wait(3)
        self.base_url = url
        self.verificationErrors = []
        self.accept_next_alert = True

    print("******* Beginning to scroll... *******")
    def scrape(self):
        driver = self.driver
        delay = 3
        driver.get(self.base_url)
        for i in range(1,6):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(3)
            
        print("******* Finished scrolling. Now creating and saving output file *******")
            
        outputFile = open(output_windows, "w")
        outputFile.write(str(driver.page_source.encode('utf-8')))
        outputFile.close()
        
        time.sleep(3)
        driver.quit()
        
        print("******* Finished saving output file... Closing Firefox instance *******")

scrape_session = Scraper()
scrape_session.init_driver()
scrape_session.scrape()
