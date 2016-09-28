# From Python
import os
import time
from time import strftime
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from sys import platform as operating_system

# From my own files
from dates_generator_v1 import dates_generator

STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2


####  STATIC INPUTS  ####################

# Define the terms that we would like to search for (ccurently restricted to two)
search_term_1 = "federal"
search_term_2 = "reserve"

# Define date range - we are moving backwards into time ∴ we begin in 2015
start_year = 2014
start_month = 4
start_day = 16
end_year = 2013
end_month = 1
end_day = 1

# Sepcify how many days should be used for one session
granularity = 15

date_list = dates_generator(start_year, start_month, start_day, end_year, end_month, end_day, granularity)

# Example of a date list from dates_generator
#date_list = ['2015-09-15', '2015-09-01', '2015-08-15', '2015-08-01','2015-07-15', '2015-07-01', '2015-06-15', '2015-06-01', '2015-05-15', '2015-05-01', '2015-04-15', '2015-04-01', '2015-03-15', '2015-03-01', '2015-02-15', '2015-02-01', '2015-01-15', '2015-01-01', '2014-12-15', '2014-12-01', '2014-11-15', '2014-11-01', '2014-10-15', '2014-10-01','2015-09-15', '2015-09-01', '2015-08-15', '2015-08-01', '2015-07-15', '2015-07-01', '2015-06-15', '2015-06-01', '2015-06-15', '2015-06-01', '2015-06-15', '2015-06-01','2015-09-15', '2015-09-01', '2015-08-15', '2015-08-01', '2015-07-15', '2015-07-01', '2015-06-15', '2015-06-01', '2015-06-15', '2015-06-01', '2015-06-15', '2015-06-01']

# Define how many times to scroll to the bottom of the page for Twitter to load more tweets (if still within the date range submitted)
# This value should allow for the date range to be completed - heuristic for now...
scroll_limit = 500

#########################################


# Define the class which performs the scrape
class Scraper:
        
    print("******* Creating Chrome instance - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
    def iterator(self, end_date, start_date):
        search_words = search_term_1 + " " + search_term_2       # For the file paths and printed summary
        search_term = search_term_1 + "%20" + search_term_2      # '%20' is a space in a url
        # Create the url - assuming we are always moving backwardsd into time
        url = 'https://twitter.com/search?q=' + search_term + '%20-abre%20lang%3Aen%20since%3A' + end_date + '%20until%3A' + start_date + '&src=typd'
        
        # Determine the location to save the output_file, depending on operating_system used 
        if operating_system == "darwin":
            output_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + search_words + '/' + start_date + '_til_' + end_date + '.txt'
        elif operating_system == "win32":
            output_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + search_words + '\\' + start_date + '_til_' + end_date + '.txt'
        
        print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

        self.init_driver(url)
        self.scrape(output_file)

        # Finish timing and print the results with a summary
        total_time_elapsed = (time.time() - start_time)
        m, s = divmod(total_time_elapsed, 60)
        h, m = divmod(m, 60)
        d, h = divmod(h, 24)

        # Print scrape summary to Python terminal
        print("\n+ ---------------- Summary ------------------ +")
        print("+ Searched term(s): " + search_words)
        print("+ Date limits: " + start_date + " to " + end_date)
        print("+ Number of scrolls: " + str(scroll_limit - 1))
        print("+ Time taken (D:HH:MM:SS): " + "%d:%02d:%02d:%02d" % (d, h, m, s) + "\n")
    
    def init_driver(self, url):
        # Define the operating system dependent paths for browser and profile
        print("******* Configuring file paths based on current operating system and beginning session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        if operating_system == "darwin":
            # OS X
            print("+ working on operating system: OS X")
            lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
            self.driver = webdriver.Firefox(firefox_profile = lite_profile)
            print("+ Opened Firefox instance")
            #output_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + search_words + '/' + start_date + '_til_' + end_date + '.txt'
        elif operating_system == "win32":
            # Windows...
            print("+ working on operating system: Windows 8")    
            chrome_options = Options()
            chrome_options.add_experimental_option( "prefs", {'profile.default_content_settings.images': 2})    # Don't load images
            self.driver = webdriver.Chrome(chrome_options=chrome_options)
            print("+ Opened Chrome instance")
            #lite_profile = webdriver.FirefoxProfile('C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox')
            #output_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + search_words + '\\' + start_date + '_til_' + end_date + '.txt'
        elif operating_system == "linux" or operating_system == "linux2":
            # Linux
            print("Error 404: Linux Found --> We aren't ready for penguins")

        self.driver.implicitly_wait(3)
        self.base_url = url
        print("+ inserted url...")
        self.verificationErrors = []
        self.accept_next_alert = True
        
    def scrape(self, output_file):
        driver = self.driver
        time.sleep(3)
        driver.get(self.base_url)
        print("******* Beginning scroll sequence - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        
        for i in range(1, scroll_limit):
            # Display prgress in current scrape cycle
            if divmod(100, i) == 0:
                print("+ Currently at scroll number: " + i)
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(2)

        print("******* Scrolling completed - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        print("******* Creating and saving output file - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")    

        # Create the directory for the result if necessary, then save the output file there...
        if not os.path.exists(os.path.dirname(output_file)):
            os.makedirs(os.path.dirname(output_file))
        with  open(output_file, "w") as file:
              file.write(driver.page_source.encode('utf-8'))

        time.sleep(5)
        driver.quit()
        
        print("******* Finished saving output file... Closing browser - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")


######  RUN  ######  RUN  ######  RUN  ######
        
# start the stopwatch and create an instance of the Scraper class
start_time = time.time()
# Make an instance of the Scraper class, which also inherently runs the functions
scrape_session = Scraper()
# Loop through the date list
for ii in range(1, len(date_list)):
    scrape_session.iterator(date_list[ii], date_list[ii -1])
    print("-------------------------------------------\n")
    print("+ Scrape session for dates: " +  date_list[ii -1] + " til " + date_list[ii] + " complete")
    progress_percentage = round((float((date_list.index(date_list[ii-1]))+1) / (len(date_list) - 1)) * 100.00, 2)
    print("+ Overall progress: " + str(date_list.index(date_list[ii])) + "/" + str(len(date_list)) + " --> " + str(progress_percentage) + "%\n")

total_time = time.time() - start_time
print(total_time)
