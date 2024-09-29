#!/usr/bin/env python
# coding: utf-8

# In[41]:


import requests
import datetime
from sqlalchemy import create_engine, Column, Integer, String, Float, TIMESTAMP
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
import logging
import unittest
from IPython.display import Image, display


# In[42]:


# Configure logging
logging.basicConfig(filename='weather_data.log', level=logging.DEBUG)

API_KEY = 'faf94359c4460b015879cd986ae00ec0'
WEATHER_URL = "http://api.openweathermap.org/data/2.5/weather"


# In[43]:


# SQLAlchemy setup
database_path = '/Users/Bella/Desktop/weather_data.db'
engine = create_engine(f'sqlite:///{database_path}')
Base = declarative_base()
Session = sessionmaker(bind=engine)
session = Session()

# Table structure
class TemperatureData(Base):
    __tablename__ = 'temperature_data'
    
    id = Column(Integer, primary_key=True, autoincrement=True)
    city = Column(String(50), nullable=False)
    date = Column(TIMESTAMP, nullable=False)
    temp_min = Column(Float, nullable=False)
    temp_max = Column(Float, nullable=False)
    description = Column(String(100), nullable=False)
    
Base.metadata.create_all(engine)


# In[44]:


def get_city_coordinates(city: str) -> tuple:
    """
    Fetches the coordinates of a city from OpenWeatherMap.

    Args:
        city: The name of the city.

    Returns:
        tuple: A tuple containing latitude and longitude.

    Raises:
        Exception: If the coordinates cannot be fetched.
    """
    geocode_url = f"http://api.openweathermap.org/geo/1.0/direct?q={city}&limit=1&appid={API_KEY}"
    response = requests.get(geocode_url)
    data = response.json()
    
    if response.status_code == 200 and len(data) > 0:
        lat = data[0]['lat']
        lon = data[0]['lon']
        print(f"Coordinates for {city}: lat = {lat}, lon = {lon}")
        return lat, lon
    else:
        raise Exception(f"Error fetching coordinates for {city}: {data.get('message', 'No data found')}")


# In[45]:


def fetch_current_temperature_data(city: str) -> tuple:
    """
    Fetches the current temperature data for a given city.

    Args:
        city: The name of the city for which to fetch temperature data.

    Returns:
        tuple: A tuple containing minimum temperature, maximum temperature,
               and weather description.

    Raises:
        Exception: If there is an API error or if the coordinates cannot be fetched.
    """
    lat, lon = get_city_coordinates(city)
    url = f"{WEATHER_URL}?lat={lat}&lon={lon}&appid={API_KEY}&units=metric"
    response = requests.get(url)
    data = response.json()

    if response.status_code != 200:
        raise Exception(f"API Error: {data.get('message')}")
    
    temp_min = data['main']['temp_min']
    temp_max = data['main']['temp_max']
    description = data['weather'][0]['description']
    
    print(f"Temperature for {city}: Min = {temp_min}, Max = {temp_max}, Description = {description}")
    return temp_min, temp_max, description


# In[46]:


def add_temperature_data(city: str, temp_min: float, temp_max: float, description: str):
    """
    Attempts to add temperature data for a given city to the database.

    Args:
        city: The name of the city.
        temp_min: Minimum temperature.
        temp_max: Maximum temperature.
        description: Weather description for the city.

    Raises:
        Exception: If there is an error during the database operation.
    """
    try:
        temp_data = TemperatureData(
            city=city,
            date=datetime.datetime.now(),
            temp_min=temp_min,
            temp_max=temp_max,
            description=description
        )
        session.add(temp_data)
        session.commit()
        logging.info(f"Data for {city} successfully stored in database.")
    except Exception as e:
        logging.error(f"Error adding data for {city}: {str(e)}")
        session.rollback() # Rollback the session on error


# In[47]:


def fetch_and_store_temp_data(city: str):
    """
    Fetches temperature data for a given city and stores it in the database.

    Args:
        city: The name of the city for which to fetch and store temperature data.

    Raises:
        Exception: If there is an error during fetching or storing data.
    """
    try:
        logging.info(f"Fetching temperature data for {city}")
        temp_min, temp_max, description = fetch_current_temperature_data(city)
        logging.info(f"Fetched data for {city}: Min = {temp_min}, Max = {temp_max}, Description = {description}")
        add_temperature_data(city, temp_min, temp_max, description)
        logging.info(f"Data for {city} successfully stored")
    except Exception as e:
        logging.error(f"Error fetching and storing data for {city}: {str(e)}")


# In[48]:


def print_temperature_data():
    """Prints all temperature records from the database."""
    all_data = session.query(TemperatureData).all()
    
    print(f"{'ID':<5} {'City':<20} {'Date':<20} {'Min Temp (°C)':<15} {'Max Temp (°C)':<15} {'Description':<20}")
    print("-" * 100)

    for data in all_data:
        formatted_date = data.date.strftime("%Y-%m-%d %H:%M:%S")
        print(f"{data.id:<5} {data.city:<20} {formatted_date:<20} {data.temp_min:<15.2f} {data.temp_max:<15.2f} {data.description:<20}")

if __name__ == '__main__':
    city = 'Stockholm'
    fetch_and_store_temp_data(city)
    print_temperature_data()


# In[49]:


if __name__ == '__main__':
    print_temperature_data()


# In[ ]:





# In[ ]:




