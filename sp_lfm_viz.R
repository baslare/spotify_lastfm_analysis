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


#### genres over time ####

genres_df <- lfm_df %>% group_by(artist_tag) %>% 
  summarise(total_played = sum(duration_ms,na.rm = T)/3600000,total_count = n()) %>% 
  arrange(desc(total_played))

genres_to_filter <- genres_df$artist_tag[1:10] %>% as.character()

lfm_df$artist_tag_other <- lfm_df$artist_tag %>% as.character()
lfm_df$artist_tag_other[which(!lfm_df$artist_tag_other %in% genres_to_filter)] <- "Other"
                                                           
gt_df <- lfm_df %>% group_by(date_dmy,artist_tag_other) %>% summarise(total_played = round(sum(duration_ms)/3600000,digits = 1),
                                                                      total_count = n()) %>% ungroup()
gt_df <- gt_df %>% filter(date_dmy > as.Date("2000-01-01-"))

gt_df$months <- gt_df$date_dmy %>% format(format="%Y - %m") %>% ym()
gt_df <- gt_df %>% group_by(months,artist_tag_other) %>% summarise(
                                                                   total_played = sum(total_played),
                                                                   total_count = sum(total_count),                                                                 )
gt_df$artist_tag_other <- gt_df$artist_tag_other %>% as.factor()

gt_df <- gt_df %>% 
  group_by(months) %>% 
  arrange(desc(total_played)) %>% 
  mutate(ratio_played = total_played/sum(total_played),
         monthly_gini = ineq::ineq(ratio_played,type = "Gini")) %>% 
  ungroup()


set.seed(5)
pal_cols <-sample(ggsci::pal_d3("category20")(12)) 

ggplot(gt_df) + 
  geom_bar(aes(x=months,y=ratio_played,fill=artist_tag_other),stat = "identity",alpha=0.7) + 
  scale_fill_manual(values = pal_cols) + 
  theme(panel.background = element_rect(fill = "#2d3033"),
        panel.grid = element_blank(),
        panel.border = element_rect(color="white",fill="transparent"),
        text=element_text(family="Noto Sans",color="#bdc3c9"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#2d3033"),
        legend.background = element_rect(fill = "#2d3033"))


ggplot(gt_df) + geom_bar(aes(x=months,y=total_played,fill=artist_tag_other),stat = "identity") + 
  scale_fill_manual(values = pal_cols) +
  theme(panel.background = element_rect(fill = "#2d3033"),
        panel.grid = element_blank(),
        panel.border = element_rect(color="white",fill="transparent"),
        text=element_text(family="Noto Sans"),
        legend.position = "bottom")

#### circle packing ####



artists_unique <- lfm_df %>% select(artist.name,artist_tag,duration_ms)
artists_unique$artist.name <- as.character(artists_unique$artist.name)
artists_unique$artist_tag <- as.character(artists_unique$artist_tag)
artists_unique$artist.name <- artists_unique$artist.name %>%  str_replace("Ghost B.C","Ghost")
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


#### songs

song_list <- lfm_df %>% 
  select(name,artist.name,artist_tag,duration_ms) %>% 
  mutate(artist_tag  = as.character(artist_tag),
         artist.name = as.character(artist.name),
         name = as.character(name))

song_count <- song_list  %>% 
  group_by(name,artist.name,artist_tag) %>% 
  summarise(count=n(),
            time_played=sum(duration_ms)/3600000) %>% ungroup()


prog_count <- song_count %>% filter(str_detect(artist_tag,"Progressive metal|Avant-garde Metal|Progressive rock|Power metal"))

prog_artists_count <- prog_count %>% group_by(artist.name) %>% summarise_if(is.numeric,sum)

#### songs pca - top 25 artists ####

all_artists_count <- song_count %>% group_by(artist.name,artist_tag) %>% summarise_if(is.numeric,sum)
top_artists <- all_artists_countc %>% arrange(desc(time_played)) %>% head(30)

songs_unique <- lfm_df %>% select(name,artist.name,artist_tag,duration_ms,danceability:tempo)
songs_unique$artist_tag <- as.character(songs_unique$artist_tag)
songs_unique$artist.name <- as.character(songs_unique$artist.name)
songs_unique$name <- as.character(songs_unique$name)

songs_unique <- songs_unique %>% filter(artist.name %in% top_artists$artist.name)
songs_unique <- songs_unique %>% distinct(name,artist.name,artist_tag,.keep_all = T)
top_artist_songs <- song_count %>% filter(artist.name %in% top_artists$artist.name)

songs_unique <- left_join(songs_unique,top_artist_songs,by=c("name","artist.name","artist_tag"))
songs_unique <- songs_unique %>% select(-c(key,mode,speechiness,instrumentalness,liveness,acousticness))

songs_unique[,5:9] <- apply(songs_unique[,5:9], MARGIN = 2, scale)
#prog_unique <- prog_unique %>% filter(time_played > 0.5)

song.pca <- prcomp(songs_unique[,5:9])
song.res <- summary(song.pca)
song.pca$rotation

song.var.explained <- song.pca$sdev^2/sum(song.res$sdev %>% sapply(function(x) x*x))

songs_unique$PC1 <- song.pca$x[,1]
songs_unique$PC2 <- song.pca$x[,2]
songs_unique$PC3 <- song.pca$x[,3]
songs_unique$PC4 <- song.pca$x[,4]




rot <- sweep(song.pca$rotation,2,song.pca$sdev,FUN="*")
rot <- as.data.frame(rot[,c("PC1","PC2")])

colnames(rot) <- c("xvar","yvar")
rot <- rot %>% mutate(angle = (180/pi)*atan(yvar/xvar),
                      hjust = (1 -1.5*sign(xvar))/2)


songs_unique <- songs_unique %>% arrange(desc(time_played))
songs_unique <- songs_unique %>% mutate()

p <- ggplot(songs_unique,aes(x=PC1,y=PC2)) + 
  geom_point(aes(size=time_played,group=name,fill=artist_tag,color=artist.name),stroke=0,alpha=0.35,shape=19) + 
  #geom_convexhull(aes(fill=artist_tag),alpha=0.1) +
  #geom_text(aes(label=artist.name),size=2,alpha=0.5,family="Noto Sans") +
  scale_fill_uchicago() ++
  scale_size_continuous(range = c(0,6)) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill="#fcede8"),
        text=element_text(family="Noto Sans"),
        panel.grid.minor = element_blank()) +
  geom_segment(data=rot,aes(x = 0,y=0,xend=2*xvar,yend=2*yvar),color="#822d11",
               arrow = arrow(length = unit(1/2, 'picas'))) +
  geom_text(data=rot,aes(label=rownames(rot),x=2*xvar,y=2*yvar,angle=angle,hjust=hjust),color="#822d11",family="Noto Sans") + labs(x=paste0("PC1: ",round(song.var.explained[1]*100,digits = 2),"% of variance"),
                                                                                                                                   y=paste0("PC2: ",round(song.var.explained[2]*100,digits = 2),"% of variance"))
plotly::ggplotly(p)


#### prog artists - pca

prog_unique <- lfm_df %>% select(name,artist.name,duration_ms,artist_tag,danceability:tempo)
prog_unique$artist_tag <- as.character(prog_unique$artist_tag)
prog_unique$artist.name <- as.character(prog_unique$artist.name)
prog_unique$name <- as.character(prog_unique$name)
prog_unique <- prog_unique %>% filter(str_detect(artist_tag,"Progressive metal|Avant-garde Metal|Progressive rock|Power metal"))
prog_unique <- prog_unique %>% distinct(name,artist.name,artist_tag,.keep_all = T)


prog_unique <- prog_unique %>% 
  group_by(artist.name,artist_tag) %>% 
  summarise_if(is.numeric,mean) %>% rename(duration_unique=duration_ms)




prog_unique <- prog_unique %>% select(-c(key,mode,speechiness,instrumentalness,liveness))
prog_unique <- left_join(prog_unique,prog_artists_count, by=c("artist.name"))


prog_unique[,3:7] <- apply(prog_unique[,3:7], MARGIN = 2, scale)
#prog_unique <- prog_unique %>% filter(time_played > 0.5)

prog.pca <- prcomp(prog_unique[,3:7])
prog.res <- summary(prog.pca)
prog.pca$rotation

prog.var.explained <- prog.pca$sdev^2/sum(prog.res$sdev %>% sapply(function(x) x*x))

prog_unique$PC1 <- prog.pca$x[,1]
prog_unique$PC2 <- prog.pca$x[,2]
prog_unique$PC3 <- prog.pca$x[,3]
prog_unique$PC4 <- prog.pca$x[,4]




rot <- sweep(prog.pca$rotation,2,prog.pca$sdev,FUN="*")
rot <- as.data.frame(rot[,c("PC1","PC2")])

colnames(rot) <- c("xvar","yvar")
rot <- rot %>% mutate(angle = (180/pi)*atan(yvar/xvar),
                      hjust = (1 -1.5*sign(xvar))/2)


prog_unique <- prog_unique %>% arrange(desc(time_played))

p2 <- ggplot(prog_unique,aes(x=PC1,y=PC2)) + 
  geom_point(alpha=0.5,aes(size=time_played,fill=artist_tag),color="#d43a06",shape=21) + 
  geom_text(aes(label=artist.name),size=2,alpha=0.5,family="Noto Sans") +
  scale_fill_uchicago() +
  scale_size_continuous(range = c(0,10)) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill="#fcede8"),
        text=element_text(family="Noto Sans"),
        panel.grid.minor = element_blank()) +
  geom_segment(data=rot,aes(x = 0,y=0,xend=2*xvar,yend=2*yvar),color="#822d11",
               arrow = arrow(length = unit(1/2, 'picas'))) +
  geom_text(data=rot,aes(label=rownames(rot),x=2*xvar,y=2*yvar,angle=angle,hjust=hjust),color="#822d11",family="Noto Sans") + labs(x=paste0("PC1: ",round(prog.var.explained[1]*100,digits = 2),"% of variance"),
                                                                                                                                   y=paste0("PC2: ",round(prog.var.explained[2]*100,digits = 2),"% of variance"))
plotly::ggplotly(p2)

#ggsave(filename = "dnm.jpeg",width = 10,height = 10,dpi = 300)

##genres - pca


genre_unique <- lfm_df %>% select(name,artist.name,duration_ms,artist_tag,danceability:tempo)
genre_unique$artist_tag <- as.character(genre_unique$artist_tag)
genre_unique <- genre_unique %>% distinct(name,artist.name,artist_tag,.keep_all = T)

duration_played <- lfm_df %>% 
  select(name,artist.name,artist_tag,duration_ms) %>% 
  group_by(name,artist.name,artist_tag) %>% 
  summarise_all(sum)

duration_played$artist_tag <- as.character(duration_played$artist_tag)

genre_unique <- genre_unique %>% 
  group_by(name,artist.name,artist_tag) %>% 
  dplyr::mutate(count=n()) %>% 
  summarise_all(mean) %>% rename(duration_unique=duration_ms)

genre_unique <- left_join(genre_unique,duration_played, by=c("artist_tag","name","artist.name"))


genre_unique <- genre_unique %>% select(-c(acousticness,key,mode,speechiness,instrumentalness,liveness))


total_played <- genre_unique %>% 
  select(artist_tag,duration_ms) %>% 
  group_by(artist_tag) %>% summarise_if(is.numeric,sum,na.rm=T)

genre_unique <- genre_unique %>% 
  group_by(artist_tag) %>% 
  summarise(danceability = sum((danceability*duration_unique)/sum(duration_unique,na.rm=T),na.rm=T),
            energy = sum((energy*duration_unique)/sum(duration_unique,na.rm=T),na.rm=T),
            loudness = sum((loudness*duration_unique)/sum(duration_unique,na.rm=T),na.rm=T),
            valence = sum((valence*duration_unique)/sum(duration_unique,na.rm=T),na.rm=T),
            tempo = sum((tempo*duration_unique)/sum(duration_unique,na.rm=T),na.rm=T),
            duration_played = sum(duration_ms,na.rm=T),
            duration_average = mean(duration_unique,na.rm=T))





genre_unique[,2:6] <- apply(genre_unique[,2:6], MARGIN = 2, scale)

genre_unique$duration_played <- genre_unique$duration_played/3600000
genre_unique <- genre_unique %>% filter(duration_played > 0.5)

genre.pca <- prcomp(genre_unique[,2:6])
summary(genre.pca)
genre.pca$rotation

genre.var.explained <- genre.pca$sdev^2/sum(genre.pca$sdev %>% sapply(function(x) x*x))

genre_unique$PC1 <- genre.pca$x[,1]
genre_unique$PC2 <- genre.pca$x[,2]
genre_unique$PC3 <- genre.pca$x[,3]
genre_unique$PC4 <- genre.pca$x[,4]



  
rot <- sweep(genre.pca$rotation,2,genre.pca$sdev,FUN="*")
rot <- as.data.frame(rot[,c("PC1","PC2")])
colnames(rot) <- c("xvar","yvar")
rot <- rot %>% mutate(angle = (180/pi)*atan(yvar/xvar),
                      hjust = (1 -1.5*sign(xvar))/2)


genre_unique <- genre_unique %>% arrange(desc(duration_played))

p3 <- ggplot(genre_unique,aes(x=PC1,y=PC2)) + 
  geom_point(alpha=0.5,aes(size=duration_played),color="#d43a06") + 
  geom_text(aes(label=artist_tag),size=2,alpha=0.5) +
  scale_fill_uchicago() +
  scale_size_continuous(range = c(2,15)) + 
  theme(legend.position = "none",
        panel.background = element_rect(fill="#fcede8"),
        text=element_text(family="Noto Sans"),
        panel.grid.minor = element_blank()) +
  geom_segment(data=rot,aes(x = 0,y=0,xend=2*xvar,yend=2*yvar),color="#822d11",
               arrow = arrow()) +
  geom_text(data=rot,aes(label=rownames(rot),x=2*xvar,y=2*yvar,angle=angle,hjust=hjust),color="#822d11",family="Noto Sans") + labs(x=paste0("PC1: ",round(genre.var.explained[1]*100,digits = 2),"% of variance"),
                                                                                                                                   y=paste0("PC2: ",round(genre.var.explained[2]*100,digits = 2),"% of variance"))
 

plotly::ggplotly(p3) 



