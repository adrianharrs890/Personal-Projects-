#!/usr/bin/env python
# coding: utf-8

# In[226]:


import csv
import requests 
import re 
from bs4 import BeautifulSoup
from urllib.request import urlopen
import numpy as np
import pandas as pd
import time
import json


# In[227]:


art = requests.get('https://www.stockmonitor.com/sp500-stocks/')
art2 = requests.get('https://www.stockmonitor.com/nasdaq-stocks/')
art3 = requests.get('https://www.stockmonitor.com/dji-stocks/')


# In[ ]:





# In[228]:


soup = BeautifulSoup(art.text, 'html.parser')
soup2 = BeautifulSoup(art2.text, 'html.parser')
soup3 = BeautifulSoup(art3.text, 'html.parser')


# In[229]:


a = []
b = []
for span in soup.find_all('a')[10:516]:
    com = span.text.strip()
    a.append((com))
    for span2 in soup.find_all('td', {'class':'text-right-always'}):
        price = span2.text.strip()
        b.append((price))
   


# In[230]:


c = []
d = []
for span in soup2.find_all('a')[10:516]:
    com2 = span.text.strip()
    c.append((com2))
    for span2 in soup2.find_all('td', {'class':'text-right-always'}):
        price2 = span2.text.strip()
        d.append((price2))


# In[231]:


e = []
f = []
for span in soup3.find_all('a')[10:516]:
    com3 = span.text.strip()
    e.append((com3))
    for span2 in soup3.find_all('td', {'class':'text-right-always'}):
        price3 = span2.text.strip()
        f.append((price3))


# In[232]:


b = b[::4]

c = c[0:103]
d = d[::4]


e = e[0:30]
f = f[::4]


df1 = pd.DataFrame(a, columns=['Company'])
df2 = pd.DataFrame(b, columns=['Price'])

df1['Price'] = df2


df11 = pd.DataFrame(c, columns=['Company'])
df22 = pd.DataFrame(d, columns=['Price'])

df11['Price'] = df22

df111 = pd.DataFrame(e, columns=['Company'])
df222 = pd.DataFrame(f, columns=['Price'])

df111['Price'] = df222


# In[166]:


df1.to_csv('sp500')
df11.to_csv('nasdaq')
df111.to_csv('dow')


