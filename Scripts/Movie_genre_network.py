#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
print (os.getcwd())
os.chdir('D:/OneDrive - The University of Kansas/2020SPRING/BSAN480/BSAN480-FinalProject/Data')
print (os.getcwd())


# In[2]:


import pandas as pd
import numpy as np


# In[5]:


genre = pd.read_csv("top_rating_genre_matrix.csv", index_col=0)


# In[7]:


genre


# In[8]:


matrix = np.asmatrix(genre)


# In[9]:


matrix_transpose = matrix.transpose()


# In[10]:


final_matrix = matrix_transpose.dot(matrix)


# In[11]:


final_matrix


# In[12]:


df = pd.DataFrame(final_matrix)


# In[13]:



## append index name
names = list(genre.columns.values.tolist()) 

df.index = names
df.columns = names


# In[14]:


df


# In[15]:


df.to_csv('top_rating_genre_adjacency_matrix.csv')


# In[16]:


import networkx as nx
from node2vec import Node2Vec


# In[17]:


adjacency_matrix = np.asmatrix(df)
g = nx.from_numpy_matrix(adjacency_matrix)


# In[18]:


g


# In[19]:


node2vec = Node2Vec(g, dimensions=5, walk_length=5, num_walks=10, workers=4)


# In[20]:


model=node2vec.fit(window = 3, min_count=1)


# In[21]:


model.wv.index2word


# In[22]:


model.wv.get_vector


# In[23]:


model.wv.save_word2vec_format('tr_embedding.csv')


# In[ ]:




