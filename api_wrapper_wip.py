# -*- coding: utf-8 -*-
"""
Created on Mon Feb 21 16:46:17 2022

@author: Efe
"""
import json
import pandas as pd
import requests


class lfm_api:
    
    api_key = str()
    root_url = "http://ws.audioscrobbler.com/2.0"
    
    def __init__(self, api_key):
        self.api_key = api_key
        
    def get_tracks(self,user,to_,extended=1,limit=200,page=1):
        
        method_url = "?method=user.getrecenttracks"
        limit = limit
        user = user
        page = page
        #from_ = from_
        extended = extended
        to_ = to_
        
        params = {"limit":limit,
                  "user":user,
                  "page":page,
                  
                  "extended":extended,
                  "to":to_,
                  "api_key":self.api_key,
                  "format":"json"}
        
        
        request_url = f"{self.root_url}/{method_url}"
        
        return(requests.get(request_url, params))
        
    def get_artist_tags(self,artist):
        
        method_url = "?method=artist.gettoptags"
        
        params = {"artist":artist,
                  "autocorrect":1,
                  "format":"json",
                  "api_key":self.api_key}
                  
        
        
        request_url = f"{self.root_url}/{method_url}"
        return(requests.get(request_url, params))
        
    
    @staticmethod
    def get_tracks_df(response_):
        response_ = response_.json()
        response_ = response_.get("recenttracks")
        response_list = response_.get("track")
        lfm_df = pd.json_normalize(response_list)
        if "@attr.nowplaying" in lfm_df.columns:
            lfm_df = lfm_df.loc[lfm_df["@attr.nowplaying"] != "true"]
            lfm_df.drop("@attr.nowplaying",axis=1,inplace=True)
        
        lfm_df["date.uts"] = lfm_df["date.uts"].apply(lambda x: int(x))
        return(lfm_df)
        
    @staticmethod
    def get_track_page_count(response_):
        response = response_.json()
        response_ = response.get("recenttracks")
        response_attr = response_.get("@attr")
        return(int(response_attr.get("totalPages")))
    
    
    
        
        