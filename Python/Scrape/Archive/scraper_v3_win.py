# Combined original code with the tutorial: http://www.marinamele.com/selenium-tutorial-web-scraping-with-selenium-and-python


import os
import time
from time import strftime
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

# Set options for chrome so that no images are loaded
chrome_options = Options()
chrome_options.add_experimental_option( "prefs", {'profile.default_content_settings.images': 2})
#driver = webdriver.Chrome(chrome_options=chrome_options)


STATUS_OK      = 0
STATUS_ERROR   = 1
STATUS_FAILURE = 2

# Create the url - assuming we are always moving backwardsd into time
search_words = "federal reserve"       # For the file paths and printed summary
search_term = "federal%20reserve"      # '%20' is a space in a url
start_date = "2014-05-15"
end_date = "2014-05-01"
url = "https://twitter.com/search?q=" + search_term + "%20-abre%20lang%3Aen%20since%3A" + end_date + "%20until%3A" + start_date + "&src=typd"

# Define how many times to scroll to the bottom of the page for Twitter to load more tweets (if still within the date range submitted)
# This value should allow for the date range to be completed - heuristic for now...
scroll_limit = 700

print("******* Configuring file paths based on current operating system *******")

# Define the operating system dependent paths
from sys import platform as operating_system
if operating_system == "darwin":
    # OS X
    lite_profile = webdriver.FirefoxProfile("/Users/nicholasmitchell/Library/Application Support/Firefox/Profiles/wrp6us7q.Lite")
    output_file = '/Users/nicholasmitchell/Volumes/Mac OS Drive/Documents/Box/scrape_raw/' + search_words + '/' + start_date + '_til_' + end_date + '.txt'
    print("working on operating system: OS x")
elif operating_system == "win32":
    # Windows...
    lite_profile = webdriver.FirefoxProfile('C:\\Users\\dev231\\AppData\\Roaming\\Mozilla\\Firefox\\Profiles\\o28gcy68.Lite-Firefox')
    output_file = 'C:\\Users\\dev231\\Box Sync\\scrape_raw\\' + search_words + '\\' + start_date + '_til_' + end_date + '.txt'
    print("working on operating system: Windows 8")    
elif operating_system == "linux" or operating_system == "linux2":
    # linux
    print("We aren't set up for a Linux system...")
    
print("******* Defining session - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

# Define the class which performs the scrape
class Scraper:
        
    print("******* Creating Chrome instance - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")

    def init_driver(self):
        self.driver = webdriver.Chrome(chrome_options=chrome_options)
        print("Opened Chrome instance")
        self.driver.implicitly_wait(3)
        self.base_url = url
        print("inserted url...")
        self.verificationErrors = []
        self.accept_next_alert = True
        
    def scrape(self):
        driver = self.driver
        time.sleep(3)
        driver.get(self.base_url)
        print("******* Beginning scroll sequence - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        
        for i in range(1, scroll_limit):
            self.driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(2)

        print("******* Scrolling completed - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")
        print("******* Creating and saving output file - " + strftime("%Y-%m-%d %H:%M:%S") + " *******")    

        # Create the firectory for the result of necessary, then put the output file there...
        if not os.path.exists(os.path.dirname(output_file)):
            os.makedirs(os.path.dirname(output_file))
        with  open(output_file, "w") as file:
            file.write(driver.page_source.encode('utf-8'))

        time.sleep(5)
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

# Print scrape summary to Python terminal
print("\n+ ---------- Summary ---------- +")
print("+ Searched term(s): " + search_words)
print("+ Date limits: " + start_date + " to " + end_date)
print("+ Number of scrolls: " + str(scroll_limit - 1))
print("+ Time taken (D:HH:MM:SS): " + "%d:%02d:%02d:%02d" % (d, h, m, s) + "\n")
