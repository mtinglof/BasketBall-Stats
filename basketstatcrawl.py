from bs4 import BeautifulSoup as soup
import itertools
import numpy as np
import os
import pandas as pd
from selenium import webdriver
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

    def set_options(self):
        path_to_extension = "C:/Users/mting/Desktop/3.31.2_0"
        chrome_options = Options()
        chrome_options.add_argument('load-extension=' + path_to_extension)
        return(chrome_options)

    def open(self):
        self.browser.create_options()
        self.browser.get(self.starting_url)

test = basketstatcrawl('https://www.basketball-reference.com/')