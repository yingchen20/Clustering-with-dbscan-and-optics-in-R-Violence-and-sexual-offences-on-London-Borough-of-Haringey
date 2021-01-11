## Part 1: Data Cleaning 
library(sf)
library(tidyverse)
library(dplyr)
library(janitor)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(rgdal)
library(spatstat)
library(here)
library(maptools)
library(GISTools)
library(tmap)
library(geojson)
library(geojsonio)
library(tmaptools)
library(hexbin)
library(ggspatial)
library(ggsn)
library(raster)
library(fpc)
library(dbscan)
library(plotrix)
library(spdep)
library(ggplot2)
library(ggpubr)
library(plotGoogleMaps) #install.packages("plotGoogleMaps", repos="http://R-Forge.R-project.org"))

## 1. Import Data 
# load crime data from met (Metropolitan Police Service) and col (City of London) in separated csv files
met20 <- read_csv("data/2020-11/2020-11-metropolitan-street.csv",
                 na = "NA")
col20 <- read_csv("data/2020-11/2020-11-city-of-london-street.csv",
                  na = "NA")
met19 <- read_csv("data/2019-11/2019-11-metropolitan-street.csv",
                 na = "NA")
col19 <- read_csv("data/2019-11/2019-11-city-of-london-street.csv",
                  na = "NA")

# load london borough shapefile
london_borough <- st_read(here::here("data",
                                  "statistical-gis-boundaries-london", 
                                  "ESRI", 
                                  "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)

# load london LSOA shapefile
london_lsoa  <- st_read(here::here("data",
                                "statistical-gis-boundaries-london",
                                "ESRI",
                                "LSOA_2011_London_gen_MHW.shp")) %>% 
  st_transform(., 27700)

## 2. Tidy Data
# combine csv files to 2019 and 2020 dataframe 
crime19 <- rbind(met19,col19)
crime20 <- rbind(met20,col20)
rm(col19,col20,met19,met20)
# data summary
summary(crime19) 
summary(crime20)
# notice  nan values in both column "Longitude" and "Latitude", 
# and the last column "Context" is completely empty.

# drop rows with NA latitude or longitude, rename and select columns
crime19 <- crime19 %>% 
  filter(Latitude != "NA" | Longitude != "NA") %>% 
  clean_names() %>% 
  dplyr::rename(police_force="falls_within") %>% 
  dplyr::select(contains("month"),
                contains("police_force"),
                contains("longitude"),
                contains("latitude"),
                contains("lsoa_code"),
                contains("lsoa_name"),
                contains("crime_type")) 
crime20 <- crime20 %>% 
  filter(Latitude != "NA" | Longitude != "NA") %>% 
  clean_names() %>% 
  dplyr::rename(police_force="falls_within") %>% 
  dplyr::select(contains("month"),
                contains("police_force"),
                contains("longitude"),
                contains("latitude"),
                contains("lsoa_code"),
                contains("lsoa_name"),
                contains("crime_type")) 

# check for completely duplicated rows 
nrow(crime19[duplicated(crime19),])  # dulicated rows (n=29504)
nrow(crime20[duplicated(crime20),])  # dulicated rows (n=33561)

# remove duplicates
crime20<- crime20 %>% 
  unique(.) 
crime19<- crime19 %>% 
  unique(.) 
# get counts of each crime type 
summary19 <- crime19 %>% 
  group_by(crime_type) %>% 
  summarise(., count=n(),) %>%
  dplyr::rename(count19="count") %>% 
  arrange(desc(count19))   
summary20 <- crime20 %>% 
  group_by(crime_type) %>% 
  summarise(., count=n(),) %>% 
  dplyr::rename(count20="count") %>% 
  arrange(desc(count20)) 
summary_crimetype <- merge.data.frame(summary19,summary20) %>% 
  mutate(change_percent=(count20-count19)/count20*100) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% 
  arrange(desc(change_percent))
rm(summary19,summary20)

# subset london violence counts 
violence20<- crime20 %>% 
  filter(., crime_type=='Violence and sexual offences')
violence19<- crime19 %>% 
  filter(., crime_type=='Violence and sexual offences')
# trim last four characters in lsoa_name column for further grouping purpose
violence20$lsoa_name = substr(violence20$lsoa_name,1,nchar(violence20$lsoa_name)-4)
violence19$lsoa_name = substr(violence19$lsoa_name,1,nchar(violence19$lsoa_name)-4)

# group violence counts by LSOA code (for generating thematic map, looking for clusters)
violence_lsoa20 <- violence20 %>% 
  group_by(lsoa_code) %>% 
  summarise(., count=n(),) %>% 
  dplyr::rename(violence_count20='count') %>% 
  arrange(desc(violence_count20)) 
violence_lsoa19 <- violence19 %>% 
  group_by(lsoa_code) %>% 
  summarise(., count=n(),) %>% 
  dplyr::rename(violence_count19='count') %>% 
  arrange(desc(violence_count19)) 

# add geometry and leave with lsoa that has violence records
violence_lsoa19 <- inner_join(london_lsoa, violence_lsoa19, by=c("LSOA11CD"="lsoa_code"))
violence_lsoa20 <- inner_join(london_lsoa, violence_lsoa20, by=c("LSOA11CD"="lsoa_code"))
summary(violence_lsoa19$violence_count19)
summary(violence_lsoa20$violence_count20)
# in 2020, 4280/4835=89% lsoa have at least 1 violence occured in December,
# compared to 4225/4835=87% in 2019.
# in 2020, # of violence offences in lsoa, mean = 3, max = 20 (Westminister),
# compared to 2019, mean = 3, max = 59 (city of london)
# choose inner join because 1. some crimes occur outside london boundary,
# 2. select inner join make sure all violence occurred within london are captured,
# also, lsoa with no violence occurred will not be counted or further reclassified.

# group violence counts by london boroughs (for generating histogram, looking at overall statistical pattern)
violence_borough19 <- violence_lsoa19 %>%
  group_by(LAD11NM) %>% 
  summarise(.,sum(violence_count19))%>% 
  dplyr::rename(violence_count19='sum(violence_count19)') %>% 
  arrange(desc(violence_count19)) %>% 
  st_drop_geometry()
violence_borough20 <- violence_lsoa20 %>%
  group_by(LAD11NM) %>% 
  summarise(.,sum(violence_count20))%>% 
  dplyr::rename(violence_count20='sum(violence_count20)') %>% 
  arrange(desc(violence_count20)) %>% 
  st_drop_geometry()
summary_vio_borough <- merge(violence_borough19, violence_borough20) %>% 
  mutate(change_percent=(violence_count20-violence_count19)/violence_count20*100) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% 
  arrange(desc(change_percent))
# with all 32 london borough + city of london, 
# Haringey has the largest grow (n=15.8%) in violence in December 2020 compared to 2019. 


# Study area = Haringey 
# clip Haringeyshapefile
Haringey <- london_lsoa %>% 
  filter(., LAD11NM=="Haringey") %>% 
  distinct(., .keep_all = TRUE)

# total Haringey violence counts 
har_vio20 <- violence20 %>% 
  filter(., lsoa_name=='Haringey ')
har_vio19 <- violence19 %>% 
  filter(., lsoa_name=='Haringey ')

# group haringey crime counts by lsoa
har_lsoa20 <- har_vio20 %>% 
  group_by(lsoa_code) %>% 
  summarise(., count=n(),) %>% 
  dplyr::rename(violence_count20='count') %>% 
  arrange(desc(violence_count20)) 
har_lsoa19 <- har_vio19 %>% 
  group_by(lsoa_code) %>% 
  summarise(., count=n(),) %>% 
  dplyr::rename(violence_count19='count') %>% 
  arrange(desc(violence_count19)) 

# add geometry 
har_lsoa20<- inner_join(london_lsoa, har_lsoa20, by=c("LSOA11CD"="lsoa_code"))
har_lsoa19<- inner_join(london_lsoa, har_lsoa19, by=c("LSOA11CD"="lsoa_code"))

# group haringey crime counts by msoa  (for joining with covid data later)
har_msoa20 <- har_lsoa20 %>% 
  group_by(MSOA11CD) %>% 
  summarise(.,sum(violence_count20))%>% 
  dplyr::rename(violence_count20='sum(violence_count20)') %>% 
  arrange(desc(violence_count20))
har_msoa19 <- har_lsoa19 %>% 
  group_by(MSOA11CD) %>% 
  summarise(.,sum(violence_count19))%>% 
  dplyr::rename(violence_count19='sum(violence_count19)') %>% 
  arrange(desc(violence_count19))

# read covid data
covid <- read_csv("data/covid_msoa.csv") %>% 
  dplyr::select(areaCode, areaName, newCasesBySpecimenDateRollingSum) %>% 
  dplyr::rename(msoa_code="areaCode", 
                msoa_name="areaName",
                newCasesSum="newCasesBySpecimenDateRollingSum")

# join covid and violence data 
har_vio_covid <- left_join(har_msoa20, covid,
                           by=c("MSOA11CD"="msoa_code"))



