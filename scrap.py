# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# Load selenium components
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

# Establish chrome driver and go to report site URL
url = "https://reportdata.mytestsite.com/transactionSearch.jsp"
driver = webdriver.Chrome()
driver.get(url)
