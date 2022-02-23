# -*- coding: utf-8 -*-
"""
Created on Tue Feb 22 12:45:51 2022

@author: Efe
"""

import os
import time
import pandas as pd
import json
import requests
import base64
import urllib.parse


class spotify:
    
    auth_url = "https://accounts.spotify.com/api/token"
    req_ses = requests.sessions.Session()
    token = str()
    
    def __init__(self, API_KEY, API_SECRET):
        self.API_KEY = API_KEY
        self.API_SECRET = API_SECRET
        
        
    def auth(self):
        auth_b64 = base64.b64encode(f"{self.API_KEY}:{self.API_SECRET}".encode("ascii")).decode("ascii")
        auth_string = f"Basic {auth_b64}"
        
        header = {"Authorization":auth_string,
                  "Content-Type":"application/x-www-form-urlencoded"}
        params = {"grant_type":"client_credentials"}
        
        response = self.req_ses.post(url=self.auth_url,
                                headers = header,
                                data = params)
        
        self.token = response.json().get("access_token")
            
    def search(self,artist="",track=""):
        

        
        search_url = 'https://api.spotify.com/v1/search?'
        
        
        
        q = f"{track} +artist:{artist}"
        
        
        headers = {"Authorization": f"Bearer {self.token}",
                   "Content-Type":"application:json"}
        
        params = {"q":q,
                  "limit":1,
                  "type":"track"}
        
        response = self.req_ses.get(url=search_url,
                                    headers = headers,
                                    params = params
                                    )
        
        return(response)
    
    def get_features_multiple_tracks(self,tracks):
        
        url = "https://api.spotify.com/v1/audio-features"
        
        track_string = ",".join(tracks)
        
        headers = {"Authorization": f"Bearer {self.token}",
                   "Content-Type":"application:json"}
        
        params = {"ids":track_string}
        
        response_ = self.req_ses.get(url=url,
                                 headers=headers,
                                 params=params)
        
        return(response_)
    
    
    def get_info_multiple_tracks(self,tracks):
        
        url = "https://api.spotify.com/v1/tracks"
        track_string = ",".join(tracks)
        
        headers = {"Authorization": f"Bearer {self.token}",
                   "Content-Type":"application:json"}
        
        params = {"ids":track_string}
        
        response_ = self.req_ses.get(url=url,
                                 headers=headers,
                                 params=params)
        
        return(response_)
    
    def get_info_multiple_artists(self,artists):
        
        
        
        url = "https://api.spotify.com/v1/artists"
        artist_string = ",".join(artists)
        
        headers = {"Authorization": f"Bearer {self.token}",
                   "Content-Type":"application:json"}
        
        params = {"ids":artist_string}
        
        response_ = self.req_ses.get(url=url,
                                 headers=headers,
                                 params=params)
        
        
        return(response_)
    
    @staticmethod
    def search_get_df(result_row):
        unpack_tracks = result_row.get("tracks")
        track_count = unpack_tracks.get("total")
        track_query = unpack_tracks.get("href")
        
        if track_count > 0:
            unpack_items = unpack_tracks.get("items")[0]
            
            
            track_id = unpack_items.get("id")
            track_name = unpack_items.get("name")
            track_popularity = unpack_items.get("popularity")
            track_duration = unpack_items.get("duration")
            
            unpack_album = unpack_items.get("album")
            unpack_artist = unpack_items.get("artists")
            artists_id = [x.get("id") for x in unpack_artist]
            artists_name = [x.get("name") for x in unpack_artist]

            album_id = unpack_album.get("id")
            album_name = unpack_album.get("name")
            album_rd = unpack_album.get("release_date")
            
            df_dict = {"track_id":track_id,
                       "track_name":track_name,
                       "track_popularity":track_popularity,
                       "track_duration":track_duration,
                       "artists_id":[artists_id],
                       "artists_name":[artists_name],
                       "album_id":album_id,
                       "album_name":album_name,
                       "album_rd":album_rd,
                       "track_query":track_query}
            return(pd.DataFrame(df_dict))
        else:
            df_dict = {"track_id":[None],
                       "track_name":[None],
                       "track_popularity":[None],
                       "track_duration":[None],
                       "artists_id":[None],
                       "artists_name":[None],
                       "album_id":[None],
                       "album_name":[None],
                       "album_rd":[None],
                       "track_query":track_query}
            return(pd.DataFrame(df_dict))
        
        
        
        
    
   
        
        
        
        
        
        
        
        
        
    