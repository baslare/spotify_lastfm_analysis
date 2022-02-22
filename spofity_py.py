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
    
   
        
        
        
        
        
        
        
        
        
    