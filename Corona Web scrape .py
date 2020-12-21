#!/usr/bin/env python
# coding: utf-8

# In[2]:


# Web scrape code as of 03/19/2020

import csv
import requests 
from bs4 import BeautifulSoup
from urllib.request import urlopen
import numpy as np
import pandas as pd
get_ipython().system('pip install openpyxl')


# In[4]:


art = requests.get('https://ncov2019.live/')


# In[ ]:


print(art.text[0:500])


# In[6]:


soup = BeautifulSoup(art.text, 'html.parser')


# In[7]:


aa = soup.find_all('span', {'class':'text--gray'})


# In[ ]:


soup.find_all("span")


# In[11]:


a = []
for span in soup.find_all('span', {'class':'text--gray'}):
    con = span.text.strip()
    a.append((con))
    print(con)
    


# In[ ]:


# Real time CoronaVirus Data 
# Loop takes a long time 
a = []
b = []
c = [] 
d = []
e = []
for span in soup.find_all('span', {'class':'text--gray'}):
    con = span.text
    a.append((con))
    for span2 in soup.find_all('span', {'class':'text--green'}):
        confirms = span2.text
        b.append((confirms))
        for span3 in soup.find_all('span', {'class':'text--red'}):
            decs = span3.text
            c.append((decs))
            for span4 in soup.find_all('span', {'class':'text--blue'}):
                rec = span4.text
                d.append((rec))
                for span5 in soup.find_all('span', {'class':'text--yellow'}):
                    ser = span5.text
                    e.append((ser))
                
    
    


# In[17]:


df1 = pd.DataFrame(a, columns=['Place'])
df2 = pd.DataFrame(b, columns=['Confirms'])
df3 = pd.DataFrame(c, columns=['Dead'])
df4 = pd.DataFrame(d, columns=['Recovered'])
df5 = pd.DataFrame(e, columns=['Serious'])


# In[ ]:


df1['Confirms'] = df2
df1['Dead'] = df3
df1['Recovered'] = df4
df1['Serious'] = df5


# In[17]:


df1


# In[23]:


data = pd.DataFrame((df1))


# In[26]:


data.to_excel('test.xlsx')


# In[20]:


data.to_csv('Corona')


# In[12]:


# confirms 
b = []
for span2 in soup.find_all('span', {'class':'text--green'}):
    confirms = span2.text
    b.append((confirms))
    
    print (confirms)


# In[ ]:





# In[13]:


# Decreased 
c = []
for span3 in soup.find_all('span', {'class':'text--red'}):
    dec = span3.text
    c.append((dec))
    print(dec)


# In[14]:


# Recovered 
d = []
for span4 in soup.find_all('span', {'class':'text--blue'}):
    rec = span4.text
    d.append((rec))
    print (rec)


# In[16]:


# Serious Cases 
e = []
for span5 in soup.find_all('span', {'class':'text--yellow'}):
    ser = span5.text
    e.append((ser))
    print (ser)


# In[ ]:




