##  Part 2: data visulization 
# histogram for violence counts in 2019 and 2020 Nov
hist19 <- 
  ggplot(violence_borough19, aes(x=violence_count19))+
  ggtitle("Violence Counts All London Borough November 2019 (Red=Mean)")+
  xlab("Recorded police counts (binwidth=30)") +
  ylab("Frequency") +
  geom_histogram(aes(y=stat(density)),
                 binwidth = 30, 
                 color="black",
                 fill="white") +
  geom_density(col="blue", size=0.5) +
  theme(plot.title = element_text(color="black", size=14, face="bold") )
hist19 <- 
  hist19 + geom_vline(aes(xintercept=mean(violence_count19, 
                                          na.rm=TRUE)),
                      color="red", 
                      linetype="dashed", 
                      size=1)+
  theme(plot.title = element_text(hjust = 0.5))

hist20 <- 
  ggplot(violence_borough20, aes(x=violence_count20))+
  ggtitle("Violence Counts All London Borough November 2020 (Red=Mean)")+
  xlab("Recorded police counts (binwidth=30)") +
  ylab("Frequency") +
  geom_histogram(aes(y=stat(density)),
                 binwidth = 30, 
                 color="black",
                 fill="white") +
  geom_density(col="blue", size=0.5) +
  theme(plot.title = element_text(color="black", size=14, face="bold") )
hist20 <- 
  hist20 + geom_vline(aes(xintercept=mean(violence_count20, 
                                          na.rm=TRUE)),
                      color="red", 
                      linetype="dashed", 
                      size=1)+
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(hist19, hist20, nrow=2)
ggsave("hist_plot.png")



## Visualize Data
tmap_mode("plot")
# 1. thematic crime counts 2019 map
a <- 
  tm_shape(violence_lsoa19) + 
  tm_fill("violence_count19", 
          breaks=c(1,5,10,20,60),
          palette="-RdBu",
          title = "LSOA Sum",
          legend.hist=FALSE) +
  tm_borders(col = NA, alpha = 0.1) +
  tm_layout(main.title="Violence Counts in London Nov 2019",
            main.title.position = "left",
            main.title.fontface = "bold",
            main.title.size = 0.9,
            title.fontface = "bold",
            legend.title.size = 0.6,
            legend.text.size = 0.5,
            legend.outside = FALSE,
            frame = FALSE,
            legend.position = c(.8,0))+
            #legend.outside.size = c(0.3,0))+
  tm_credits("(a)", position=c(0,0.85), size=1) +
  tm_compass(type = "4star", size = 3, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "bottom")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) 

# 2. thematic crime count 2020 map
b <- 
  tm_shape(violence_lsoa20) + 
  tm_fill("violence_count20",
          breaks=c(1,5,10,20,60),
          palette="-RdBu",
          title = "LSOA Sum",
          legend.hist=FALSE) +
  tm_borders(col = NA, alpha = 0.1) +
  tm_layout(main.title="Violence Counts in London Nov 2020",
            main.title.position = "left",
            main.title.fontface = "bold",
            main.title.size = 0.9,
            title.fontface = "bold",
            legend.title.size = 0.6,
            legend.text.size = 0.5,
            #legend.hist.size = 0.4,
            legend.outside = FALSE,
            legend.position = c(.8,0),
            #legend.outside.size = c(0.3,0),
            frame = FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1)+
  tm_compass(type = "4star", size = 3, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "bottom")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) 

# 3.thematic haringey msoa violence counts 
d <- 
  tm_shape(har_vio_covid) + 
  tm_fill("violence_count20", 
          #style="jenks",
          breaks=c(1,5,10,20,30),
          palette="-RdBu",
          title = "MSOA Sum",
          legend.hist=FALSE) +
  tm_borders(col = NA, alpha = 0.5) +
  tm_layout(main.title="Violence Counts at Haringey Nov 2020",
            main.title.position = "left",
            main.title.fontface = "bold",
            main.title.size = 0.9,
            title.fontface = "bold",
            legend.title.size = 0.6,
            legend.text.size = 0.5,
            #legend.hist.size = 0.4,
            legend.outside = FALSE,
            legend.position = c(0.8,0),
            #legend.outside.size = c(0.3,0),
            frame = FALSE) +
  tm_credits("(c)", position=c(0,0.85), size=1) +
  tm_compass(type = "4star", size = 3, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "bottom")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) 

# 4. thematic haringey msoa violence counts
c <- 
  tm_shape(har_msoa19) + 
  tm_fill("violence_count19", 
          #style = "jenks",
          breaks=c(1,5,10,20,30),
          palette="-RdBu",
          title = "MSOA Sum",
          legend.hist=FALSE) +
  tm_borders(col = NA, alpha = 0.5) +
  tm_layout(main.title="Violence Counts at Haringey Nov 2019",
            main.title.position = "left",
            main.title.fontface = "bold",
            main.title.size = 0.85,
            title.fontface = "bold",
            legend.title.size = 0.6,
            legend.text.size = 0.5,
            #legend.hist.size = 0.4,
            legend.outside = FALSE,
            legend.position = c(.8,0),
            #legend.outside.size = c(0.3,0),
            frame = FALSE) +
  tm_credits("(d)", position=c(0,0.85), size=1)+
  tm_compass(type = "4star", size = 3, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "bottom")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) 
themtiac_map <- tmap_arrange(a, b, c, d, ncol=2, nrow=2)
themtiac_map
tmap_save(themtiac_map, 'thematic_map.png')

# covid map
covid <- 
  tm_shape(har_vio_covid) + 
  tm_fill("newCasesSum", 
          #style="jenks",
          breaks=c(1,5,10,20,30),
          palette="-RdBu",
          title = "MSOA Sum",
          legend.hist=FALSE) +
  tm_borders(col = NA, alpha = 0.5) +
  tm_layout(main.title="Covid New Cases Sum at Haringey Nov 2020",
            main.title.position = "left",
            main.title.fontface = "bold",
            main.title.size = 0.9,
            title.fontface = "bold",
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            #legend.hist.size = 0.4,
            legend.outside = FALSE,
            legend.position = c(0.8,0),
            #legend.outside.size = c(0.3,0),
            frame = FALSE) +
  tm_credits("(c)", position=c(0,0.85), size=1) +
  tm_compass(type = "4star", size = 3, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "bottom")) +
  tm_scale_bar(color.dark = "gray60",
               position = c("left", "bottom")) 
tmap_save(covid, "covid map.png")


# study area plot
# first transform dataframe to spatial data
har_vio20_sf <- har_vio20 %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>% 
  distinct(., geometry,.keep_all = TRUE) %>% 
  st_transform(., 27700) 
# transform to sp objoect
har_vio20_sp <- har_vio20_sf %>%
  as(., 'Spatial') 
# calculate geographic standard distance 
# code from: https://www.r-bloggers.com/2015/05/introductory-point-pattern-analysis-of-open-crime-data-in-london/
mean_centerX <- mean(har_vio20_sp@coords[,1])
mean_centerY <- mean(har_vio20_sp@coords[,2])
standard_deviationX <- sd(har_vio20_sp@coords[,1])
standard_deviationY <- sd(har_vio20_sp@coords[,2])
standard_distance <- sqrt(sum(((har_vio20_sp@coords[,1]-mean_centerX)^2+
                                 (har_vio20_sp@coords[,2]-mean_centerY)^2))/(nrow(har_vio20_sp)))
# point map (DROPPED)
london_outline <- london_borough$geometry
point_map <- 
  plot(har_vio20_sp,pch="+", col="blue",cex=1, 
       main="Violence Counts at Haringey November 2020 \n(Ellipse=Standard Distance)") +
  plot(london_outline, add=T) +
  points(mean_centerX,mean_centerY,col="red",pch=16) +
  draw.ellipse(mean_centerX,mean_centerY,a=standard_deviationX,b=standard_deviationY,border="red",lwd=2)
# function to transform geometry to (x,y) coordinates from 
# https://maczokni.github.io/crimemapping_textbook_bookdown/more-on-thematic-maps.html
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
har_vio20_sf <- sfc_as_cols(har_vio20_sf, c("longitude", "latitude"))

# study area heat map
heat_map <- 
  ggplot(har_vio20_sf, aes(x = longitude, y = latitude))+
  ggtitle("Violence Density at Haringey (Nov 2020)")+
  theme(plot.title=element_text(hjust = 0.5))+
  labs(caption = "Copyright OpenStreetMap contributors",
       x="Longitude",y="Latitude")+
  annotation_map_tile(zoomin = 0) + 
  stat_density2d(aes(fill = ..level.., 
                     alpha = ..level..), 
                 geom = "polygon") +  
  scale_fill_gradientn(colours = c("white","red"), 
                       name = "Density")
ggsave("studyArea.png")





