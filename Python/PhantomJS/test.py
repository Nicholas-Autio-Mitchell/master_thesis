# class test():
#     def setUp(self):
#         # app.config['TESTING'] = True
#         # app.config['CSRF_ENABLED'] = False

#         self.driver = webdriver.PhantomJS()

#     def test_frontpage(self):
#         driver = self.driver
#         driver.get('localhost:5000http://www.google.com')
#         print driver.page_source


# test = test()
# test.setUp()
# test.test_frontpage()
        
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

desired_cap = {
        'phantomjs.page.settings.loadImages' : True,
        'phantomjs.page.settings.resourceTimeout' : 10000,
        'phantomjs.page.settings.userAgent' : "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:40.0) Gecko/20100101 Firefox/40.0"
}
#dcap["phantomjs.page.settings.userAgent"] = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:40.0) Gecko/20100101 Firefox/40.0")

driver = webdriver.PhantomJS(executable_path= "/usr/local/bin/phantomjs", desired_capabilities=desired_cap)
driver.set_window_size(1024, 768)
driver.get('https://google.com/')

driver.save_screenshot("testing.png")
driver.page_source("source_code.txt")
element = driver.find_element_by_xpath('--*[@id=-gbqfq-]')
element.send_keys('testing')
element.send_keys(Keys.ENTER)
