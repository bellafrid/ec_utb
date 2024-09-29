#!/usr/bin/env python
# coding: utf-8

# In[5]:


import pytest
from temp_data import get_city_coordinates, fetch_current_temperature_data

def test_get_city_coordinates():
    lat, lon = get_city_coordinates("Stockholm")
    assert lat is not None
    assert lon is not None

def test_fetch_current_temperature_data():
    city = "Stockholm"
    temp_min, temp_max, description = fetch_current_temperature_data(city)
    assert temp_min is not None
    assert temp_max is not None
    assert isinstance(description, str)


# In[ ]:




