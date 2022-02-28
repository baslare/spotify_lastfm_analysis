require(jsonlite)
require(tidyverse)
require(ggraph)
require(igraph)
require(lubridate)
require(extrafont)
require(ggsci)

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

ggplot(df_audio_sub) + geom_tile(aes(x=week,y=weekday,fill=instrumentalness)) + coord_equal() + scale_fill_material("pink")

ggplot(df_audio_sub) + geom_point(aes(x=week,y=weekday,size=log(1+duration_sum/1000))) + coord_equal() + scale_size(range = c(0,5))

#### circle packing ####



artists_unique <- lfm_df %>% select(artist.name,artist_tag,duration_ms)
artists_unique$artist.name <- as.character(artists_unique$artist.name)
artists_unique$artist_tag <- as.character(artists_unique$artist_tag)
artists_unique$artist.name <- str_replace(artists_unique$artists.name,"Ghost B.C","Ghost")
artists_unique <- artists_unique %>% group_by(artist_tag,artist.name) %>% summarise(duration_total = sum(duration_ms))
artists_unique$duration_total <- artists_unique$duration_total/3600000
artists_unique <- artists_unique %>% filter(duration_total > 4)



genre_unique <- data.frame(group="root",subitem=unique(artists_unique$artist_tag),size=0)
colnames(artists_unique) <- colnames(genre_unique)


edge_df <- rbind(genre_unique,artists_unique)
edge_df <- edge_df %>% distinct(group,subitem,size)
edge_df$size <- ceiling(edge_df$size)

vertex_df <- edge_df %>% select(subitem,size) %>% add_row(subitem="root",size=0)
vertex_df$text_color <- ifelse(vertex_df$subitem == "root","transparent",ifelse(vertex_df$size == 0,"white","black"))
vertex_df$display_name <- ifelse(vertex_df$subitem == "root","",ifelse(vertex_df$size == 0,vertex_df$subitem,ifelse(vertex_df$size >10, vertex_df$subitem,"")))
vertex_df$display_name <- ifelse((vertex_df$display_name %>% str_detect("\\s")) & (nchar(vertex_df$display_name) > 9), str_replace(vertex_df$display_name,"\\s","\n"), vertex_df$display_name)

df_graph <- graph_from_data_frame(edge_df,vertices = vertex_df)

ggraph(df_graph,layout = "circlepack",weight=size) + 
  geom_node_circle(aes(fill=depth),show.legend = FALSE,color="transparent")  + 
  geom_node_text(aes(label=display_name,color=as.factor(depth)),show.legend = FALSE,family="Noto Sans") +
  scale_color_manual(values = c("transparent","white","#4a1c09")) + scale_fill_viridis_c(option = "B",alpha = 0.6) +
  theme(text=element_text(family="Noto Sans"))

ggsave("efe_last_fm.jpeg",height = 18,width = 18,dpi=500)

####pca

songs_unique <- lfm_df %>% select(artist.name,duration_ms,artist_tag,danceability:tempo)
songs_unique <- songs_unique %>% group_by(artist.name,artist_tag) %>% dplyr::mutate(count=n()) %>% summarise_all(mean)
songs_unique <- songs_unique %>% select(-c(acousticness,key,mode,speechiness))
songs_unique$artist.name <- as.character(songs_unique$artist.name)
songs_unique$artist_tag <- as.character(songs_unique$artist_tag)

songs_unique[,4:10] <- apply(songs_unique[,4:10], MARGIN = 2, scale)



songs.pca <- prcomp(songs_unique[,4:10])
summary(songs.pca)
songs.pca$rotation



songs_unique$PC1 <- songs.pca$x[,1]
songs_unique$PC2 <- songs.pca$x[,2]
songs_unique$PC3 <- songs.pca$x[,3]
songs_unique$PC4 <- songs.pca$x[,4]



ggplot(songs_unique,aes(x=PC1,y=PC3,size=count)) + 
  geom_point(alpha=0.5,aes(color=as.factor(artist_tag))) + 
  scale_size_continuous(range = c(0,3)) + 
  theme(legend.position = "none")

