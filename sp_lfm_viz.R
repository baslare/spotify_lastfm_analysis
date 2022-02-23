require(jsonlite)
require(tidyverse)
require(ggraph)
require(igraph)
require(lubridate)

parse_sci <- function(x){
  
  chk <- x %>% str_detect("[A-z]")
  
  if(chk){
    spl <- x %>% str_split("e-")
    spl_1 <- spl[[1]][1] %>% as.numeric()
    spl_2 <- spl[[1]][2] %>% as.numeric()
    
    return(spl_1*(10^(-spl_2)))
  }else{
    return(x %>% as.numeric())
  }
  
  
}


lfm_df <- fromJSON("spot_lfm_final.json") 
lfm_df <- as_tibble(lfm_df)


lfm_df_cols <- lapply(lfm_df, function(x) x %>% str_detect("[A-z]+"))
lfm_df_cols <- sapply(lfm_df_cols,sum)
lfm_df_cols <- lfm_df_cols[lfm_df_cols <10000]
lfm_df_cols <- names(lfm_df_cols)
lfm_df_cols <- lfm_df_cols[-3]




lfm_df <- lfm_df %>% mutate_at(lfm_df_cols,.funs = function(x) sapply(x,parse_sci))
lfm_df$date_dmy <- lubridate::parse_date_time( lfm_df$`date.#text`,"dmy HM",select_formats = "%d %C %Y") %>% lubridate::as_date()



#### audio characteristics over time ####

audio_chars <- lfm_df_cols[4:14]


df_audio <- FALSE

df_audio <- lfm_df %>% group_by(date_dmy) %>% summarise_at(audio_chars,~weighted.mean(.x,duration_ms))
df_audio_helper <- lfm_df %>% group_by(date_dmy) %>% summarise(duration_sum = sum(duration_ms))

df_audio <- left_join(df_audio,df_audio_helper,by="date_dmy")

days_master <- seq(dmy("12-02-2013"),dmy("22-02-2022"),by="days")
days_master <- data.frame(date_dmy = days_master)

df_audio <- left_join(days_master,df_audio,by="date_dmy")
df_audio[is.na(df_audio)] <- 0

df_audio$weekday <- weekdays(df_audio$date_dmy)
df_audio$week <- ceiling(as.period(df_audio$date_dmy - min(df_audio$date_dmy))/(weeks(1)))

df_audio_sub <- df_audio[df_audio$date_dmy > dmy("22-02-2021"),]

ggplot(df_audio_sub) + geom_tile(aes(x=week,y=weekday,fill=instrumentalness)) + coord_equal()

ggplot(df_audio_sub) + geom_point(aes(x=week,y=weekday,size=log(1+duration_sum/1000))) + coord_equal() + scale_size(range = c(0,5))

#### circle packing ####

