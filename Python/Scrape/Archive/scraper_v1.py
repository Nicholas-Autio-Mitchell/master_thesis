# Combined original code with the tutorial: http://www.marinamele.com/selenium-tutorial-web-scraping-with-selenium-and-python

import logging
import sys
import datetime
import time
from time import strftime

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
search_words = "\"Dow Jones\""    # For the summary to be printed on completion
search_term = 'Dow%20Jones'       # '%20' is a space in a url
start_date = '2014-06-01'
end_date = '2014-05-30'
url = 'https://twitter.com/search?q=' + search_term + '%20-abre%20lang%3Aen%20since%3A' + end_date + '%20until%3A' + start_date + '&src=typd'

# Define how many times to scroll to the bottom of the page for Twitter to load more tweets (if still within the date range submitted)
scroll_limit = 6

#############  operating system dependent links  #####################
## ------------- System paths ----------------------------------------
#output_windows = 'C:\\Users\\dev231\\Box Sync\\' + start_date + '_til_' + end_date + '.txt'
output_mac = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/' + start_date + '_til_' + end_date + '.txt'
##
## ------------- Use a minimalistic firefox profile ------------------
#lite_profile_windows = webdriver.FirefoxProfile('"C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox\\"')
lite_profile_mac = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
######################################################################

print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")


class Scraper:
        
    print("******* Creating Firefox instance - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
    
    def init_driver(self):
        self.driver = webdriver.Firefox(lite_profile_mac)
        self.driver.implicitly_wait(30)
        self.base_url = url
        self.verificationErrors = []
        self.accept_next_alert = True

    def scrape(self):
        driver = self.driver
        delay = 10
        driver.get(self.base_url)

        print("******* Beginning scroll sequence - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        
        for i in range(1, scroll_limit):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(1)

        print("******* Scrolling completed - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        print("******* Creating and saving output file - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")    

        outputFile = open(output_mac, "w")
        outputFile.write(driver.page_source.encode('utf-8'))
        outputFile.close()

        time.sleep(1)
        driver.quit()
        
        print("******* Finished saving output file... Closing Firefox instance - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")


# start the stopwatch and create an instance of the Scraper class
start_time = time.time()
scrape_session = Scraper()
scrape_session.init_driver()
scrape_session.scrape()

# Finish timing and print the results with a summary
total_time_elapsed = (time.time() - start_time)
m, s = divmod(total_time_elapsed, 60)
h, m = divmod(m, 60)
d, h = divmod(h, 24)

# Print results to Python terminal
print("\n+---------- Summary ----------+")
print("+ Searched for: " + search_words)
print("+ In date range: " + start_date + " to " + end_date)
print("+ Number of scrolls: " + str(scroll_limit - 1))
print("+ Time taken (D:HH:MM:SS): " + "%d:%02d:%02d:%02d" % (d, h, m, s) + "\n")
