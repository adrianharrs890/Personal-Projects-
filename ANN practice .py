#!/usr/bin/env python
# coding: utf-8

# In[58]:



# Import all needed libraries and sublibraries

import tensorflow as tf

from keras.models import Sequential
from keras.layers import Dense
from keras.optimizers import Adam
from keras.callbacks import EarlyStopping

import pandas as pd
import numpy as np 

import sklearn
from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score

from matplotlib import pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')
get_ipython().run_line_magic('config', "InlineBackend.figure_format='retina'")


# In[59]:


runescape = pd.read_csv('~/Desktop/rs1.csv')


# In[60]:


# Preprocessing is done in R
runescape.head()


# In[ ]:





# In[25]:


# Splitting the data set 
X_train, X_test, y_train, y_test = train_test_split(baseball.iloc[:,2:14],baseball[['year']], test_size=0.2)


# In[26]:


# Scaling inputs 

X_train = preprocessing.scale(X_train)
X_test = preprocessing.scale(X_test)


# In[35]:


# Model

model = Sequential()
model.add(Dense(4, input_shape=(12,), activation='relu'))
model.add(Dense(15, activation='relu'))
model.add(Dense(15, activation='relu'))
model.add(Dense(1,))
model.compile(Adam(lr=0.003), 'mean_squared_error')

# Runs model for 2000 iterations and assigns this to 'history'
history = model.fit(X_train, y_train, epochs = 100, validation_split = 0.2, verbose = 0)

# Plots 'history'
history_dict=history.history
loss_values = history_dict['loss']
val_loss_values=history_dict['val_loss']
plt.plot(loss_values,'bo',label='training loss')
plt.plot(val_loss_values,'r',label='training loss val')


# In[ ]:





# In[16]:





# In[48]:





# In[49]:





# In[ ]:





# In[ ]:




