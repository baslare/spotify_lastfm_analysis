# -*- coding: utf-8 -*-
"""
Created on Tue Feb 22 12:44:00 2022

@author: Efe
"""

import os

os.chdir("C:/Users/Efe/Desktop/Projeler/spotify_lastfm")
from api_wrapper_wip import lfm_api
from spofity_py import spotify
import time
import pandas as pd
import json
import requests


lfm_df = pd.read_json("last_fm_raw.json")


sp = spotify(API_KEY="your_api_key",API_SECRET="your_api_secret")
sp.auth()


lfm_df_unique = lfm_df.drop_duplicates(subset=["artist.name","name"])


asd = sp.search(lfm_df_unique["artist.name"].iloc[0],lfm_df_unique["name"].iloc[0])


result_list = []

i = 0


while i < 50:
    
    result_list.append(sp.search(lfm_df_unique["artist.name"].iloc[i],lfm_df_unique["name"].iloc[i],lfm_df_unique["album.#text"].iloc[i]))
    
    i +=1
    

result_list[5].json()
