violence19 <- violence19 %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>% 
  distinct(., geometry,.keep_all = TRUE) %>% 
  st_transform(., 27700)
vio_haringey19 <- violence19[Haringey,]

vio_haringey_sp19 <- vio_haringey19 %>%
  as(., 'Spatial') 
vio_haringey_ppp19 <- ppp(x=vio_haringey_sp19@coords[,1],
                     y=vio_haringey_sp19@coords[,2],
                     window=window)

# Ripley's K 
png("Kplot19.tiff", units="in", width=6, height=4, res=300)
Kest(vio_haringey_ppp19, correction="border") %>%
  plot(xlim=c(0,220), ylim=c(0,350000),
       main="Ripley's K Test \n Haringey violence counts in Nov 2019 (n=362)")
dev.off()

# DBSCAN
#first extract the points from the spatial points data frame
violence_subpoints19 <- vio_haringey_sp19 %>%
  geometry(.)%>%
  as.data.frame()
violence_subpoints19%>%
  dbscan::kNNdistplot(.,k=4)
# k-nearest neighbor distance plot
# knee = 400
db19 <- violence_subpoints19%>%
  dbscan::kNNdistplot(.,k=4) %>% 
  title(main="4-nearst Neighbor Distance Plot \n Haringey violence counts in Nov 2019 (knee=400)") %>% 
  abline(h = 400, col = "red", lty = 2)


#dbscan plot
db19_ep400 <- violence_subpoints19 %>%
  fpc::dbscan(.,eps = 400, MinPts = 4)
# Convex hull plot 
library(factoextra)
theme_set(theme_minimal())
db19_pl400 <- fviz_cluster(db19_ep400, violence_subpoints19, 
                           geom = "point",
                           stand = FALSE, labelsize = NA,
                           outlier.pointsize = .8,
                           #xlab="x", ylab="y",
                           main="DBSCAN Cluster Hulls 2019 \n (ep=400, MinPts=4)")
db19_pl400 <- db19_pl400 + 
  theme(plot.title=element_text(size = 12, hjust = 0.5, face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  coord_equal() 
db19_pl400

dbplot <- ggarrange(db20_pl350, db20_pl300, db19_pl400 + rremove("x.text"),  
          labels = c("(a)", "(b)", "(c)"),
          vjust=5,
          ncol = 2, nrow = 2)
ggsave("dbplot_all.png")

