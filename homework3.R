unzip("/Users/matthewheath/Desktop/bikes.zip")
library(lubridate)
library(tidyverse)
## Question 1
baytrip<-function(path){
  data1= read_csv(path)
  type_convert<-c("trip_id", "start_station_id", "end_station_id", "bike_number")
  data1[type_convert]<-lapply(data1[type_convert], factor)
  data1$start_date=ymd_hms(data1$start_date)
  data1$end_date=ymd_hms(data1$end_date)
  saveRDS(data1, "sf_bikeshare_trips.rds")
}
baytrip("sf_bikeshare_trips.csv")
baytrip_data<-read_rds("sf_bikeshare_trips.rds")
sapply(baytrip_data, class)

baystations<- function(path){
  data2=read_csv("sf_bike_share_stations.csv")
  data2$station_id=as.factor(data2$station_id)
  data2$installation_date=ymd(data2$installation_date)
  saveRDS(data2, "sf_bike_share_stations.rds")
}
baystations("sf_bike_share_stations.csv")
baystations_data<-read_rds("sf_bike_share_stations.rds")
sapply(baystations_data, class)

library(ggmap)
library(ggrepel)
sf<-subset(baystations_data, baystations_data$landmark == "San Francisco")
sf1<-unique(sf)
start<-as.data.frame(table(baytrip_data$start_station_id))
sf2<-merge(x=start, y=sf1, by.x="Var1", by.y= "station_id")
#eliminated duplicate stations and combined them with frequencies from trip data
sf_map<-ggmap(get_map("San Francisco", zoom= 13))
sf_map +
  geom_point(data= sf2, aes(x=longitude, y=latitude, size=Freq), alpha= I(1/2))+
  geom_text_repel(data=sf2, aes(x=longitude, y=latitude, label=name), size=2)+
  labs(x="longitude", y="latitude", title="Bike Stations in SF")

## Question 3
latrip<-function(path){
  files <- dir(path)
  la_data <- lapply(files, read_csv)
  colnames(la_data[[4]])[5]<-"start_station_id"
  colnames(la_data[[5]])[5]<-"start_station_id"
  colnames(la_data[[4]])[8]<-"end_station_id"
  colnames(la_data[[5]])[8]<-"end_station_id"
  la_data_frame <- do.call(rbind, la_data)
  type_convert<-c("trip_id", "start_station_id", "end_station_id")
  la_data_frame[type_convert]<-lapply(la_data_frame[type_convert], factor)
  la_data_frame$start_time=mdy_hm(la_data_frame$start_time)
  la_data_frame$end_time=mdy_hm(la_data_frame$end_time)
  la_data_frame$start_time[which(is.na(la_data_frame$start_time))]<-la_data[[4]]$start_time
  la_data_frame$end_time[which(is.na(la_data_frame$end_time))]<-la_data[[4]]$end_time
  saveRDS(la_data_frame, "la_metro_trips.rds")
}
latrip("/Users/matthewheath/Desktop/hw3data/")
latrip_data<-read_rds("la_metro_trips.rds")
sapply(latrip_data, class)

lastations<- function(path){
  la_data2=read_csv(path)
  la_data2$Station_ID=as.factor(la_data2$Station_ID)
  la_data2$Go_live_date=mdy(la_data2$Go_live_date)
  la_data2$Go_live_date[which(is.na(la_data2$Go_live_date))]<-as.Date("2016-07-07")
  saveRDS(la_data2, "metro-bike-share-stations-2017-10-20.rds")
}
lastations("metro-bike-share-stations-2017-10-20.csv")
lastations_data<-read_rds("metro-bike-share-stations-2017-10-20.rds")
sapply(lastations_data, class)


la<-subset(lastations_data, lastations_data$Region == "DTLA")
startla<-as.data.frame(table(latrip_data$start_station_id, latrip_data$start_lat, latrip_data$start_lon))
startla<-startla[-row(startla)[startla==0],]
la1<-merge(x=startla, y=la, by.x="Var1", by.y= "Station_ID")
la1$Var2<-as.numeric(levels(la1$Var2))[la1$Var2]
la1$Var3<-as.numeric(levels(la1$Var3))[la1$Var3]
la1$Var2<-round(la1$Var2, digits=4)
la1$Var3<-round(la1$Var3, digits=3)
#allows us to do match same stations with slightly different coordinates
la2<-with(la1, aggregate(list(Freq=Freq), list(Station_ID=Var1, Station_Name=Station_Name, latitude=Var2, longitude=Var3), sum))
la_map<-ggmap(get_map("Downtown Los Angeles", zoom=13))
la_map +
  geom_point(data= la2, aes(x=longitude, y=latitude, size=Freq), alpha= I(1/2))+
  geom_text_repel(stat='identity', data=la2, aes(x=longitude, y=latitude, label= Station_Name), size=2)+
  labs(x="longitude", y="latitude", title="Bike Stations in LA")

library(geosphere)
baytrip_data$start_time<-format(baytrip_data$start_date, format="%H:%M:%S")
baytrip_data$end_time<-format(baytrip_data$end_date, format="%H:%M:%S")
baytrip_data$start_time<-as.POSIXct(baytrip_data$start_time, format="%H:%M:%S")
baytrip_data$end_time<-as.POSIXct(baytrip_data$end_time, format="%H:%M:%S")
#makes the date the same and allows us to look only at time of day
ggplot(baytrip_data, aes(start_time, duration_sec))+
  geom_point()+ylim(0, 3000000)+
  labs(x="Time", y="Duration", title="Duration by Time")

ggplot(baytrip_data, aes(start_time))+
  geom_histogram(bins=24)+
  labs(x="Time", y="Frequency", title="Trips by Time")

baytrip_data2<-merge(x=baytrip_data, y=baystations_data[,c("station_id", "latitude", "longitude")], by.x="start_station_id", by.y="station_id")
baytrip_data2<-unique(baytrip_data2)
colnames(baytrip_data2)[names(baytrip_data2)=="latitude"]<-"start_lat"
colnames(baytrip_data2)[names(baytrip_data2)=="longitude"]<-"start_long"
baytrip_data2<-merge(x=baytrip_data2, y=baystations_data[,c("station_id", "latitude", "longitude")], by.x="end_station_id", by.y="station_id")
baytrip_data2<-unique(baytrip_data2)
colnames(baytrip_data2)[names(baytrip_data2)=="latitude"]<-"end_lat"
colnames(baytrip_data2)[names(baytrip_data2)=="longitude"]<-"end_long"
baytrip_data2$distance<-distGeo(matrix(c(baytrip_data2$start_long, baytrip_data2$start_lat), ncol=2), matrix(c(baytrip_data2$end_long, baytrip_data2$end_lat), ncol=2))
ggplot(baytrip_data2, aes(start_time, distance))+
  geom_point()+
  labs(x="Time", y="Distance (in meters)", title="Distance by Time")

latrip_data$starting_time<-format(latrip_data$start_time, format="%H:%M:%S")
latrip_data$ending_time<-format(latrip_data$end_time, format="%H:%M:%S")
latrip_data$starting_time<-as.POSIXct(latrip_data$starting_time, format="%H:%M:%S")
latrip_data$ending_time<-as.POSIXct(latrip_data$ending_time, format="%H:%M:%S")
ggplot(latrip_data, aes(starting_time, duration))+
  geom_point()+
  labs(x="Time", y="Duration", title="LA Duration by Time")

ggplot(latrip_data, aes(starting_time))+
  geom_histogram(bins=24)+
  labs(x="Time", y="Frequency", title="LA Trips by Time")

latrip_data$distance<-distGeo(matrix(c(latrip_data$start_lon, latrip_data$start_lat), ncol=2), matrix(c(latrip_data$end_lon, latrip_data$end_lat), ncol=2))
ggplot(latrip_data, aes(starting_time, distance))+
  geom_point()+ylim(0, 50000)+
  labs(x="Time", y="Distance (in meters)", title="LA Distance by Time")

sf3<-merge(x=baytrip_data, y=sf1, by.x="start_station_id", by.y= "station_id")
colnames(sf3)[names(sf3)=="latitude"]<-"start_lat"
colnames(sf3)[names(sf3)=="longitude"]<-"start_long"
sf3<-merge(x=sf3, y=sf1[,c("station_id", "latitude", "longitude")], by.x="end_station_id", by.y="station_id")
colnames(sf3)[names(sf3)=="latitude"]<-"end_lat"
colnames(sf3)[names(sf3)=="longitude"]<-"end_long"
sf3$bearing<-bearing(matrix(c(sf3$start_lon, sf3$start_lat), ncol=2), matrix(c(sf3$end_lon, sf3$end_lat), ncol=2))
sf3$start_time<-format(sf3$start_date, format="%H:%M:%S")
sf3$start_time<-as.POSIXct(sf3$start_time, format="%H:%M:%S")
sf3$start_time<-round(sf3$start_time, "hour")
sf3$start_time<-format(sf3$start_time, format="%H:%M:%S")
ggplot(sf3, aes(start_time, bearing))+
  geom_boxplot()+
  labs(x="Time", y="Bearing", title="Bearing by Time")+
  theme(axis.text.x=element_text(angle=90,hjust=1))



