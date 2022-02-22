# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 17:35:14 2022

@author: Efe
"""

import os

os.chdir("C:/Users/Efe/Desktop/Projeler/spotify_lastfm")
from api_wrapper_wip import lfm_api
import time
import pandas as pd
import json
import requests

lfm = lfm_api()


response_ = lfm.get_tracks("baslare", to_=time.time())
total_page = lfm_api.get_track_page_count(response_)
lfm_df = lfm_api.get_tracks_df(response_)
next_time = min(lfm_df["date.uts"])



i = 0

while i < total_page:
    response_ = lfm.get_tracks("baslare", to_=next_time)
    r_df = lfm_api.get_tracks_df(response_)
    next_time = min(r_df["date.uts"])
    lfm_df = lfm_df.append(r_df)
    print(f"{i+1} of {total_page}")
    i += 1
    

lfm_df.drop(columns=["loved","image","streamable","artist.image"], inplace=True)
lfm_df.reset_index(inplace=True)

with open("last_fm_raw.json",  "w", encoding="utf-8") as file:
    lfm_df.to_json(file)
    