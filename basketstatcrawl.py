import pandas as pd
from pandas import ExcelWriter
import html5lib
import numpy as np
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import WebDriverException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select
import winsound
import time

class basketstatcrawl:
    def __init__(self, starting_url):
        self.starting_url = starting_url
        self.browser = webdriver.Chrome("C:/Users/mting/Documents/chromedriver.exe", chrome_options=self.set_options())
        self.writer = pd.ExcelWriter('PlayerExport.xlsx')

    def set_options(self):
        path_to_extension = "C:/Users/mting/Desktop/3.31.2_0"
        chrome_options = Options()
        chrome_options.add_argument('load-extension=' + path_to_extension)
        return(chrome_options)

    def getplayertable(self):
        self.browser.create_options()
        self.browser.get(self.starting_url)
        names = pd.read_excel('MILvSAC-NOvLAL.xlsx', sheet_name='ALL')
        [self.open(player) for player in names['Name']]

    def open(self, name):
        try:
            searchbar = self.browser.find_element_by_xpath("//input[@tabindex='1']")
            searchbar.send_keys(name)
            searchbar.send_keys(Keys.RETURN)
            try:
                self.browser.find_element_by_xpath("//tr[@id='per_game.2019.clone']").click()
            except NoSuchElementException:
                try:
                    maintable = self.browser.find_element_by_xpath("//div[@id='all_per_game']")
                    tablelist = maintable.find_elements(By.TAG_NAME, "th")
                    tablelist[-2].click()
                except NoSuchElementException:
                    foundplayers = self.browser.find_elements_by_partial_link_text(name)
                    if(len(foundplayers)>1):
                        winsound.PlaySound("SystemExclamation", winsound.SND_ALIAS)
                        print("Looks like a two name error. Press enter when done")
                        input()
                    else:
                        self.browser.find_element_by_partial_link_text(name).click()
                    try:
                        self.browser.find_element_by_xpath("//tr[@id='per_game.2019.clone']").click()
                    except NoSuchElementException:
                        try:
                            maintable = self.browser.find_element_by_xpath("//div[@id='all_per_game']")
                            tablelist = maintable.find_elements(By.TAG_NAME, "th")
                            tablelist[-2].click()
                        except NoSuchElementException:
                            winsound.PlaySound("SystemExclamation", winsound.SND_ALIAS)
                            print("Something is wrong")
                            input()
            table = self.browser.find_element_by_xpath("//table[@id='pgl_basic']")
            df = pd.read_html(table.get_attribute("outerHTML"))[0]
            df['FG'].replace('', np.nan, inplace=True)
            df['FG'].replace('FG', np.nan, inplace=True)
            df.dropna(subset=['FG'], inplace=True)
            df[['FG', '3P', 'FT', 'TRB', 'AST', 'STL', 'BLK', 'TOV']] = df[['FG', '3P', 'FT', 'TRB', 'AST', 'STL', 'BLK', 'TOV']].apply(pd.to_numeric)
            df.to_excel(self.writer, name)
            self.writer.save()
        except NoSuchElementException:
            winsound.PlaySound("SystemExclamation", winsound.SND_ALIAS)
            print("Were we 404'ed?")
            error = input()
            print("y or n")
            if (error == "y"):
                self.browser.implicitly_wait(10)
                self.browser.refresh()
                self.open(name)
            else:
                pass

        
test = basketstatcrawl('https://www.basketball-reference.com/')
test.getplayertable()