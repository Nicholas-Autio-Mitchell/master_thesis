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
search_term = 'Dow%20Jones'
start_date = '2014-06-01'
end_date = '2014-05-30'
url = 'https://twitter.com/search?q=' + search_term + '%20-abre%20lang%3Aen%20since%3A' + end_date + '%20until%3A' + start_date + '&src=typd'
#output_windows = 'C:\\Users\\dev231\\Box Sync\\' + start_date + '_til_' + end_date + '.txt'
output_mac = '/Volumes/Mac OS Drive/Documents/Box/' + start_date + '_til_' + end_date + '.txt'

# Use a minimalistic firefox profile
#lite_profile_windows = webdriver.FirefoxProfile('"C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox\\"')
lite_profile_mac = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
#lite_profile = webdriver.FirefoxProfile("C:\Users\dev231\AppData\Roaming\Mozilla\Firefox\Profiles\o28gcy68.Lite-Firefox")

print("defining functions")

def init_driver(self):
    self.driver = webdriver.Firefox(lite_profile_mac)
    self.driver.implicitly_wait(30)
    self.base_url = url
#    self.base_url =  "https://twitter.com/search?q=Dow%20Jones%20-abre%20lang%3Aen%20since%3A2015-08-28%20until%3A2015-08-30&src=typd"
    self.verificationErrors = []
    self.accept_next_alert = True
    return driver

def scrape(self):
    driver = self.driver
    delay = 3
    driver.get(self.base_url)
    for i in range(1,550):
        self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
        time.sleep(1)


print("Fininshed defining functions. Now creating and saving file")
        
outputFile = open(output_mac, "w")
outputFile.write(driver.page_source)
outputFile.close()
    
time.sleep(5)
driver.quit()
