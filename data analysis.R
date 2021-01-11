## Part 3: point pattern analysis on study area
# transform to ppp object
window <- as.owin(Haringey)
har_vio20_ppp <- ppp(x=har_vio20_sp@coords[,1],
                     y=har_vio20_sp@coords[,2],
                     window=window)
# Ripley's K 
# r=170 (2020)
png("Kplot.tiff", units="in", width=6, height=4, res=300)
Kest(har_vio20_ppp, correction="border") %>%
  plot(xlim=c(0,250), ylim=c(0,150000),
       main="Ripley's K Test \n Haringey violence counts in Nov 2020 (n=430)")
dev.off()

# DBSCAN
#first extract the points from the spatial points data frame
violence_subpoints20 <- har_vio20_sp %>%
  geometry(.)%>%
  as.data.frame()
# k-nearest neighbor distance plot
png("DBplot20.tiff", units="in", width=6, height=4, res=300)
db20 <- violence_subpoints20%>%
  dbscan::kNNdistplot(.,k=6) %>% 
  title(main="4-nearst Neighbor Distance Plot \n (ep=350, MinPts=6, knee=350") %>% 
  abline(h = 350, col = "red", lty = 2)
dev.off()

#dbscan plot
# knee=350
db20_ep350 <- violence_subpoints20 %>%
  fpc::dbscan(.,eps = 350, MinPts = 6)
theme_set(theme_minimal())
db20_pl350 <- fviz_cluster(db20_ep350, violence_subpoints20, 
                           geom = "point",
                           stand = FALSE, labelsize = NA,
                           outlier.pointsize = .8,
                           #xlab="x", ylab="y",
                           main="DBSCAN Cluster Hulls 2020 \n (ep=350, MinPts=6)")
db20_pl350 <- db20_pl350 + 
  theme(plot.title=element_text(size = 12, hjust = 0.5, face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_equal()
db20_pl350
## Noise points are black. 
## Note that density-based clusters do not need to be convex and 
## thus noise points and points of another cluster may lie within the convex hull 
## of a different cluster.

# try different parameters 
theme_set(theme_minimal())
db20_ep300 <- violence_subpoints20 %>% 
  fpc::dbscan(.,eps = 300, MinPts = 4)
db20_pl300 <- fviz_cluster(db20_ep300, violence_subpoints20, 
                           geom = "point",
                           stand = FALSE, labelsize = NA,
                           outlier.pointsize = .8,
                           #xlab="x", ylab="y",
                           main="DBSCAN Cluster Hulls 2020 \n (ep=300, MinPts=4)")
db20_pl300 <- db20_pl300 + 
  theme_grey() + 
  theme(plot.title=element_text(size = 12, hjust = 0.5, face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_equal() 
db20_pl300


# OPTICS
op20 <- optics(violence_subpoints20, eps = 1000, minPts = 10)
op20  # available filed has order, 
# OPTICS is an augmented ordering algorithm, 
# which stores the computed order of the points it found in the order element of the returned object.
plot(op20) 
# The reachability plot shows the reachability distance for points ordered by OPTICS. 
# Valleys represent potential clusters separated by peaks. 
# Very high peaks may indicate noise points. 

## EXTRACT METHOD
# OPTICS has two primary cluster extraction methods 
# using the ordered reachability structure it produces. 
#A DBSCAN-type clustering can be extracted using extractDBSCAN() 
# by specifying the global neighborhood size ε . 
# The reachability plot shows three peaks,
# i.e., points with a high reachability-distance. 
# These points indicate boundaries between three clusters. 
# A threshold that separates the four clusters can be visually determined. 
# In this case we use eps_cl of 300. 

# global neighborhood size = 300
op20_300 <- extractDBSCAN(op20, eps_cl = 300)  # set same limit as DBSCAN
png("reach300.tiff", units="in", width=6, height=4, res=300)
plot(op20_300, main="Reachability Plot \n global neighborhood size (eps_cl)=300")
dev.off()
# hull plot
hullplot(violence_subpoints20, op20_300,
         cex = TRUE,
         main="OPTICS Cluster Hulls 2020 \n  (eps_cl) = 300") 

# try global neighborhood size = 500
op20_500 <- extractDBSCAN(op20, eps_cl = 500)  # set same limit as DBSCAN
# reachability plot 
png("reach500.tiff", units="in", width=6, height=4, res=300)
plot(op20_500, main="Reachability Plot \n global neighborhood size (eps_cl)=500")
dev.off()


## OPTICS map
violence_subpoints20<- violence_subpoints20 %>%
  mutate(op_cluster300=op20_300$cluster)

hull_300 <- violence_subpoints20 %>%
  group_by(op_cluster300) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

hull_300 <- hull_300 %>%
  filter(op_cluster300 >=1)

# add a basemap
Haringeybb <- Haringey %>%
  st_transform(., 4326)%>%
  st_bbox()
library(OpenStreetMap)
basemap <- OpenStreetMap::openmap(c(51.56466414,-0.17128513),c(51.61121422,-0.04145125),
                                  zoom=NULL,
                                  "stamen-toner")
# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")
# plot
final_plot <- autoplot.OpenStreetMap(basemap_bng) + 
  geom_point(data=violence_subpoints20, 
             aes(coords.x1,coords.x2, 
                 fill=op_cluster300),
             size=0.3)+
  geom_polygon(data = hull_300, 
               aes(coords.x1,coords.x2, 
                   group=op_cluster300,
                   fill=op_cluster300),
               alpha = 1,
               fill = "red") +
  theme(legend.position = "none") +
  ggtitle("DBSCAN-Extracted Violence Cluster at Haringey Nov.2020")+
  theme(plot.title=element_text(hjust = 0.5, face="bold"))+
  labs(caption = "Copyright OpenStreetMap contributors",
       x="Meter",y="Meter")
final_plot
ggsave("OPTICS_plot.png")


