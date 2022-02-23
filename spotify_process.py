# -*- coding: utf-8 -*-
"""
Created on Tue Feb 22 12:44:00 2022

@author: Efe
"""

import os
###change the working directory / if needed
os.chdir("C:/Users/Efe/Desktop/Projeler/spotify_lastfm_analysis")
from api_wrapper_wip import lfm_api
from spofity_py import spotify
import time
import pandas as pd
import json
import requests
import pickle
import urllib.parse
import numpy as np


###bring in the last.fm dataset
lfm_df = pd.read_json("last_fm_raw.json")
lfm_df_unique = lfm_df.drop_duplicates(subset=["artist.name","name"])
lfm_df_unique.reset_index(inplace=True)


###authenticate yourself with the spotify api
sp = spotify(API_KEY="your_api_key",API_SECRET="your_api_secret")
sp.auth()


##initiate an instantce of the lfm_api class

lfm_ = lfm_api(api_key="your_api_key")


### search spotify, use last-fm scrobbles to create a personal list of songs played

result_list = []

i = 0

temp_holder = []
lfm_df_unique.loc[:,"track_query"] = ""

while i < len(lfm_df_unique['artist.name']):
    
    
    response_ = sp.search(lfm_df_unique["artist.name"].iloc[i],lfm_df_unique["name"].iloc[i])
    lfm_df_unique["track_query"].iloc[i] = response_.request.path_url
    
    if(response_.status_code == 200):
        temp_holder.append(response_)
    elif(response_.status_code == 429):
        print(response_.status_code)
        break
    
    if i % 50 == 0:
        
        print(f"{i+1} of {len(lfm_df_unique['artist.name'])}")
        result_list = result_list + temp_holder
        temp_holder = []
        time.sleep(2)
        
    i +=1
    
    
result_list = result_list + temp_holder


#save the data
with open("spot.pickle", "wb") as f:
            pickle.dump(result_list, f, protocol=pickle.HIGHEST_PROTOCOL)
         

#reload the dataset if needed                 
#with open("spot.pickle", "rb") as f:
#            result_list = pickle.load(f)
    
            
###tidy up the data
            

result_list = [x.json() for x in result_list]           
result_list_df = [sp.search_get_df(x) for x in result_list]
result_list_df = pd.concat(result_list_df)
result_list_df.reset_index(inplace=True)

master_df = pd.concat([result_list_df,lfm_df_unique],axis=1)

result_list_exists = result_list_df[[x != None for x in result_list_df.track_id]]
result_list_exists.reset_index(inplace=True)
result_list_exists.drop(["index"],inplace=True, axis=1)



###get audio features of the tracks - 100 per query
interval_1 = np.arange(0,result_list_exists.shape[0],100)
interval_2 = np.arange(99,result_list_exists.shape[0],100)
interval_2 = np.append(interval_2,result_list_exists.shape[0]-1)


features_list = []

for x,y in zip(interval_1,interval_2):
    rspnse = sp.get_features_multiple_tracks(result_list_exists.loc[x:y,"track_id"])
    features_list.append(rspnse)
 
    
#save the data
with open("spot_features.pickle", "wb") as f:
            pickle.dump(features_list, f, protocol=pickle.HIGHEST_PROTOCOL)
            

#reload the dataset if needed
#with open("spot_features.pickle", "rb") as f:
#            features_list = pickle.load(f)
    
features_list = [x.json() for x in features_list]
features_list = [x.get("audio_features") for x in features_list]
features_list = list(np.concatenate(features_list))
features_list = [pd.DataFrame(x,    index=[0]) for x in features_list]
features_df = pd.concat(features_list)
features_df.reset_index(inplace=True)

###get track info - 50 per query

interval_1 = np.arange(0,result_list_exists.shape[0],50)
interval_2 = np.arange(49,result_list_exists.shape[0],50)
interval_2 = np.append(interval_2,result_list_exists.shape[0]-1)


info_list = []

for x,y in zip(interval_1,interval_2):
    rspnse = sp.get_info_multiple_tracks(result_list_exists.loc[x:y,"track_id"])
    info_list.append(rspnse)
    
info_list = [x.json() for x in info_list]
info_list = [x.get("tracks") for x in info_list]
info_list = list(np.concatenate(info_list))


###get artist info - 50 per query


artist_id = list(result_list_exists["artists_id"])
artist_id = np.concatenate(artist_id)
artist_id = list(artist_id)

artist_list = []


interval_1 = np.arange(0,len(artist_id),50)
interval_2 = np.arange(49,len(artist_id),50)
interval_2 = np.append(interval_2,len(artist_id)-1)

for x,y in zip(interval_1,interval_2):
    rspnse = sp.get_info_multiple_artists(artist_id[x:y+1])
    artist_list.append(rspnse)
    
#save the data
with open("spot_artists.pickle", "wb") as f:
            pickle.dump(artist_list, f, protocol=pickle.HIGHEST_PROTOCOL)
 
#with open("spot_artists.pickle", "rb") as f:
#            artist_list = pickle.load(f)
            

artist_list = [x.json() for x in artist_list]
artist_list = [x.get("artists") for x in artist_list]
artist_list = list(np.concatenate(artist_list))
artist_list = [pd.json_normalize(x) for x in artist_list]
artist_df = pd.concat(artist_list)
            
            


### merge all into one master df

#get rid of useless columns
features_df = features_df.drop(["index","type","uri","track_href","analysis_url"],  axis=1)
features_df = features_df.rename(columns={"id":"track_id"})

#create master_df
            
master_df = pd.concat([result_list_df,lfm_df_unique],axis=1)
master_df = master_df.drop_duplicates(subset=["artist.name","name"])
master_df = master_df[[x != None for x in result_list_df.track_id]]


#simplify artists
master_df = master_df.assign(artist_name = [x[0] for x in list(master_df["artists_name"])])
master_df = master_df.assign(artist_id = [x[0] for x in list(master_df["artists_id"])])
master_df = master_df.drop(["index","track_duration","level_0","index"],  axis=1)
master_df.reset_index(inplace=True, drop=True)

###even though the most prudent way would be to just left_join, but some spotify track ids correspond to multiple last.fm tracks
###which makes sense, because we searched for some song-artist title combinations.
#master_df = pd.merge(master_df,features_df,how="left",left_on="track_id",right_on="id")
master_df = pd.concat([master_df,features_df],axis=1)

artist_df = artist_df[["id","genres","followers.total","popularity"]]
artist_df = artist_df.rename(columns={"id":"artist_id"})
artist_df = artist_df.drop_duplicates(subset="artist_id")

master_df = master_df.merge(artist_df,how="left",on="artist_id")


#ascertaining the names of the columns that are going to be in the final data frame
ft_col = features_df.columns.to_list()
ft_col.remove("track_id")

at_col = artist_df.columns.to_list()
at_col.remove("artist_id")

artist_unique = list(master_df["artist.name"].drop_duplicates())
#get artist top tags
artist_tags = [lfm_.get_artist_tags(x) for x in artist_unique]
artist_tags_list = [x.json() for x in artist_tags]
artist_tags_list = [x.get("toptags").get("tag") for x in artist_tags_list]
artist_tags_list = [x[0].get("name") if len(x) > 0 else [] for x in artist_tags_list]

artist_tags_df = pd.DataFrame({"artist.name":artist_unique,
                               "artist_tag":artist_tags_list})

master_df = master_df[["track_id","track_name","album_id","album_name","album_rd","artist_id","artist_name","name","artist.name","track_query","track_popularity","artists_id","artists_name"]+ft_col+at_col]
master_df = master_df.loc[:,~master_df.columns.duplicated()]

#populate the playlist record with spotify characteristics

lfm_df = lfm_df.merge(master_df,how="left",on=["name","artist.name"])
lfm_df.reset_index(inplace=True, drop=True)
lfm_df = lfm_df[[not pd.isna(x) for x in lfm_df.track_id]]

lfm_df = lfm_df.merge(artist_tags_df,how="left",on="artist.name")


with open("spot_lfm_final.json", "w",encoding="utf-8") as file:
            lfm_df.to_json(file, force_ascii=False)

    

