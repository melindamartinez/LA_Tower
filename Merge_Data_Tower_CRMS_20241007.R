library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
setwd("D:/USGS Computer/Data/CRMS Data")

# Increase the penalty for scientific notation
options(scipen = 999)

###### CRMS Data #####
# CRMS 3166 - Freshwater Site (US-LA2)
hydro_3166<-read.csv("CRMS_3166_Hydro_Hourly/CRMS_3166_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)
head(hydro_3166)
str(hydro_3166)

colnames(hydro_3166) <-c( "Station ID"                                 ,"Date",                         
                                               "Time"                            ,"Time Zone",                                 
                                               "Sensor Environment"                         ,"Raw Water Temperature" ,            
                                               "Adjusted Water Temperature"         ,"Raw Specific Conductance"  ,        
                                               "Adjusted Specific Conductance"      ,"Raw Salinity" ,                       
                                               "Adjusted Salinity"                    ,"Raw Water Level" ,                     
                                               "Adjusted Water Level"                  ,"Raw Water Elevation to Marsh",         
                                               "Adjusted Water Elevation to Marsh"     ,"Raw Water Elevation to Datum" ,        
                                               "Adjusted Water Elevation to Datum"     ,"Raw Marsh Mat Elevation",              
                                               "Adjusted Marsh Mat Elevation to Datum" ,"Geoid",                                     
                                               "Raw Battery"                            ,"Adjusted Battery",                      
                                               "Raw Wind Speed"                       ,"Adjusted Wind Speed",                 
                                               "Raw Wind Direction"               ,"Adjusted Wind Direction",         
                                               "Raw Velocity"                      ,"Adjusted Velocity",                
                                               "Raw Precipitation"              ,"Adjusted Precipitation",           
                                               "Raw Air Pressure"                ,"Adjusted Air Pressure",          
                                               "Raw Total Chlorophyll"       ,"Adjusted Total Chlorophyll", 
                                               "Raw Dissolved Oxygen"        ,"Adjusted Dissolved Oxygen" , 
                                               "Raw pH"                          ,"Adjusted pH",                    
                                               "Raw Turbidity"                        ,"Adjusted Turbidity",                  
                                               "Raw Discharge"               ,"Adjusted Discharge",         
                                               "Organization Name"                          ,"Comments",                                  
                                               "Latitude"                                   ,"Longitude" )

hydro_3166$DateTime<-paste(hydro_3166$Date,hydro_3166$Time, sep=" ")
hydro_3166$DateTime<-as.POSIXct(hydro_3166$DateTime,format= "%m/%d/%Y %H:%M:%S")

hydro_3166_points<- hydro_3166 %>% select(`Station ID`,Latitude,Longitude,Date)
hydro_3166_points$Date<- as.Date(hydro_3166_points$Date,format="%m/%d/%Y")

head(hydro_3166_points %>% filter(Date > "2024-02-28"))
# Extract the year
hydro_3166_points$Year <- format(hydro_3166_points$Date, "%Y")
# Extract the month
hydro_3166_points$Month <- format(hydro_3166_points$Date, "%m")

hydro_3166_points$YearMonth<- paste(hydro_3166_points$Year,hydro_3166_points$Month,hydro_3166_points$`Station ID`,sep="-")

hydro_3166_sub<- hydro_3166_points %>% select(YearMonth)
head(hydro_3166_sub)
unique(hydro_3166_sub)

# M100 - 202303-202312
# M01 - 200803-201810
# H01 - 200803-202405

hydro_3166_points_shp<-data.frame(lapply(hydro_3166_points %>% select(`Station ID`,Latitude,Longitude), unique))
hydro_3166_points_shp$Start <- c("200803","200803","202303")
hydro_3166_points_shp$End <- c("202405","201810","202312")

library(sf)
# Convert data frame to an sf object
sf_3166 <- st_as_sf(hydro_3166_points_shp, coords = c("Longitude", "Latitude"), crs = 4326)

# View the sf object
print(sf_3166)
plot(sf_3166)
#st_write(sf_3166,"C:/Users/melindamartinez/OneDrive - DOI/Data/CRMS Data/CRMS_3166_Hydro_Hourly/Hydro_Stations_3166.shp")

# CRMS 0224 - Salt Marsh Site (US-LA3)
hydro_0224<-read.csv("CRMS_0224_Hydro_Hourly/CRMS_0224_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)

head(hydro_0224)
str(hydro_0224)
colnames(hydro_0224)<-c( "Station ID"                                 ,"Date",                         
                         "Time"                            ,"Time Zone",                                 
                         "Sensor Environment"                         ,"Raw Water Temperature" ,            
                         "Adjusted Water Temperature"         ,"Raw Specific Conductance"  ,        
                         "Adjusted Specific Conductance"      ,"Raw Salinity" ,                       
                         "Adjusted Salinity"                    ,"Raw Water Level" ,                     
                         "Adjusted Water Level"                  ,"Raw Water Elevation to Marsh",         
                         "Adjusted Water Elevation to Marsh"     ,"Raw Water Elevation to Datum" ,        
                         "Adjusted Water Elevation to Datum"     ,"Raw Marsh Mat Elevation",              
                         "Adjusted Marsh Mat Elevation to Datum" ,"Geoid",                                     
                         "Raw Battery"                            ,"Adjusted Battery",                      
                         "Raw Wind Speed"                       ,"Adjusted Wind Speed",                 
                         "Raw Wind Direction"               ,"Adjusted Wind Direction",         
                         "Raw Velocity"                      ,"Adjusted Velocity",                
                         "Raw Precipitation"              ,"Adjusted Precipitation",           
                         "Raw Air Pressure"                ,"Adjusted Air Pressure",          
                         "Raw Total Chlorophyll"       ,"Adjusted Total Chlorophyll", 
                         "Raw Dissolved Oxygen"        ,"Adjusted Dissolved Oxygen" , 
                         "Raw pH"                          ,"Adjusted pH",                    
                         "Raw Turbidity"                        ,"Adjusted Turbidity",                  
                         "Raw Discharge"               ,"Adjusted Discharge",         
                         "Organization Name"                          ,"Comments",                                  
                         "Latitude"                                   ,"Longitude" )

hydro_0224$DateTime<-paste(hydro_0224$Date,hydro_0224$Time, sep=" ")
hydro_0224$DateTime<-as.POSIXct(hydro_0224$DateTime,format= "%m/%d/%Y %H:%M:%S")
#hydro_0224 %>% filter(DateTime > "2024-02-28*" & DateTime < "2024-03-01*")

hydro_0224_points<- hydro_0224 %>% select(`Station ID`,Latitude,Longitude)
hydro_0224_points<-data.frame(lapply(hydro_0224_points, unique))

min(hydro_0224$DateTime,na.rm=TRUE)
max(hydro_0224$DateTime,na.rm=TRUE)

hydro_0224_points$Start<-c("200606")
hydro_0224_points$End<-c("202404")
# Convert data frame to an sf object
sf_0224 <- st_as_sf(hydro_0224_points, coords = c("Longitude", "Latitude"), crs = 4326)

# View the sf object
print(sf_0224)
plot(sf_0224)
#st_write(sf_0224,"C:/Users/melindamartinez/OneDrive - DOI/Data/CRMS Data/CRMS_0224_Hydro_Hourly/Hydro_Stations_0224.shp")

##### AmeriFlux Data #####

la2_amf<-read.csv("D:/USGS Computer/Data/LA_FluxTower/Ameriflux_Download/AMF_US-LA2_BASE-BADM_4-5/AMF_US-LA2_BASE_HH_4-5.csv",
                      skip = 2, header = TRUE)
la3_amf<-read.csv("D:/USGS Computer/Data/LA_FluxTower/Ameriflux_Download/AMF_US-LA3_BASE-BADM_2-5/AMF_US-LA3_BASE_HH_2-5.csv",
                      skip = 2, header = TRUE)
# Recent Data (not in AmeriFlux) 
la2_2024<-read.csv("D:/USGS Computer/Data/LA_FluxTower/Tower Data/LA02/AmeriFlux_Processed/US-LA2_HH_202401010000_202501010000.csv")
la3_2024<-read.csv("D:/USGS Computer/Data/LA_FluxTower/Tower Data/LA03/AmeriFlux_Processed/US-LA3_HH_202401010000_202501010000.csv")

str(la2_amf)
str(la3_amf)

# Need to remove the "_1_1_1"
names(la2_2024) <- gsub("_1_1_1$", "", names(la2_2024))
names(la3_2024) <- gsub("_1_1_1$", "", names(la3_2024))
names(la3_amf) <- gsub("_1_1_1$", "", names(la3_amf))

# Find where they mismatch
setdiff(names(la3_amf),names(la3_2024))
setdiff(names(la2_amf),names(la2_2024))

# Filling in Missing Columns to match
la3_amf$CUSTOM_RSSI_77_MEAN<- -9999
la3_2024$SWC<- -9999
la3_2024$TS<- -9999

la2_amf$CUSTOM_RSSI_77_MEAN<- -9999
la2_2024$TS<- -9999
la2_2024$VPD_PI<- -9999

# Reorder 2024 data
la2_2024_ordered<-la2_2024[,names(la2_amf)]
la3_2024_ordered<-la3_2024[,names(la3_amf)]

# Checking Data
colnames(la2_2024_ordered)
colnames(la2_amf)
colnames(la3_2024_ordered)
colnames(la3_amf)

# Merging rows
la2_all<-rbind(la2_amf,la2_2024_ordered)
la3_all<-rbind(la3_amf,la3_2024_ordered)

# Editing dataframe
la2_all$DateTime<-ymd_hm(la2_all$TIMESTAMP_START)
la3_all$DateTime<-ymd_hm(la3_all$TIMESTAMP_START)

# Remove last two digits
la2_all$TIMESTAMP_START <- la2_all$TIMESTAMP_START %/% 100
la3_all$TIMESTAMP_START <- la3_all$TIMESTAMP_START %/% 100

# Checking Data available
ggplot(la2_all %>% filter(FC != -9999 & TIMESTAMP_START > "2020-01-01"), aes(x=TIMESTAMP_START, y=FC)) + geom_point()
ggplot(la3_all %>% filter(FC != -9999), aes(x=TIMESTAMP_START, y=FC)) + geom_point()

##### Merging Tower and CRMS Hydro Data #####
# LA2 Site - CRMS 3166
head(hydro_3166)
colnames(hydro_3166)
unique(hydro_3166$`Station ID`)
# Find column numbers that start with "Adjusted"
adjusted_columns <-  select(hydro_3166, starts_with("Adjusted")) %>% names()

range(la2_all$DateTime)

hydro_3166_select_H01<-hydro_3166 %>% filter(`Station ID` == "CRMS3166-H01" & DateTime > "2011-01-01*") %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns))

unique(hydro_3166$`Station ID`)
range(hydro_3166_select_H01$DateTime,na.rm = TRUE)

hydro_3166_select_H01$Year<-year(hydro_3166_select_H01$DateTime)
hydro_3166_select_H01$Month<-format(hydro_3166_select_H01$DateTime, "%m")
hydro_3166_select_H01$Day<-format(hydro_3166_select_H01$DateTime, "%d")
hydro_3166_select_H01$Time<-format(hydro_3166_select_H01$DateTime, "%H")

hydro_3166_select_H01$TIMESTAMP_START<-as.numeric(paste0(hydro_3166_select_H01$Year,hydro_3166_select_H01$Month,
                                                         hydro_3166_select_H01$Day,hydro_3166_select_H01$Time))

la2_hydro<-merge(la2_all,hydro_3166_select_H01,by="TIMESTAMP_START", all = TRUE)

colnames(la2_hydro)
str(la2_hydro)

plot(FCH4~`Adjusted Water Level`, la2_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Level`, la2_hydro %>% filter(FC != -9999))

# LA3 Site - CRMS 0224
head(hydro_0224)
colnames(hydro_0224)
unique(hydro_0224$`Station ID`)
# Find column numbers that start with "Adjusted"
adjusted_columns_224 <-  select(hydro_0224, starts_with("Adjusted")) %>% names()

range(la3_all$DateTime)

hydro_0224_select_H01<-hydro_0224 %>% filter(DateTime >= "2018-12-31*") %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns_224))

unique(hydro_0224$`Station ID`)
range(hydro_0224_select_H01$DateTime,na.rm = TRUE)

hydro_0224_select_H01$Year<-year(hydro_0224_select_H01$DateTime)
hydro_0224_select_H01$Month<-format(hydro_0224_select_H01$DateTime, "%m")
hydro_0224_select_H01$Day<-format(hydro_0224_select_H01$DateTime, "%d")
hydro_0224_select_H01$Time<-format(hydro_0224_select_H01$DateTime, "%H")

hydro_0224_select_H01$TIMESTAMP_START<-as.numeric(paste0(hydro_0224_select_H01$Year,hydro_0224_select_H01$Month,
                                                         hydro_0224_select_H01$Day,hydro_0224_select_H01$Time))

la3_hydro<-merge(la3_all,hydro_0224_select_H01,by="TIMESTAMP_START", all = TRUE)

colnames(la3_hydro)
str(la3_hydro)

plot(FCH4~`Adjusted Water Level`, la3_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Level`, la3_hydro %>% filter(FC != -9999))
#### Adding GridMet Climate Data #####
la2_climate<-read_excel("C:/Users/melmart/Documents/Data/Climate Engine Data/LA_Tower/GridMet_LA2_All.xlsx",sheet = "Data")
la3_climate<-read_excel("C:/Users/melmart/Documents/Data/Climate Engine Data/LA_Tower/GridMet_LA3_All.xlsx",sheet = "Data")

# LA2 Site - Climate
head(la2_climate)
str(la2_climate)

la2_climate<-la2_climate %>% filter(Date > "2010-12-31*")

la2_climate$Year<- format(la2_climate$Date, "%Y")
la2_climate$Month<- format(la2_climate$Date, "%m")
la2_climate$Day<- format(la2_climate$Date, "%d")

la2_hydro$TIMESTAMP_START<- la2_hydro$TIMESTAMP_START %/% 100

la2_climate$TIMESTAMP_START<- as.numeric(paste0(la2_climate$Year,
                                                la2_climate$Month,la2_climate$Day))

la2_hydro_climate <- merge(la2_hydro,la2_climate %>% select(-c(Year,Month,Day)),by = "TIMESTAMP_START",all=TRUE)

names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.y"] <- "WS_Grid"

la2_hydro_climate$DateTime.x<-as.character(la2_hydro_climate$DateTime.x)
la2_hydro_climate$Year<-as.character(la2_hydro_climate$Year)
la2_hydro_climate$Month<-as.character(la2_hydro_climate$Month)
la2_hydro_climate$Time<-as.character(la2_hydro_climate$Time)

#st_write(la2_hydro_climate,"C:/Users/melmart/DocumentsTime#st_write(la2_hydro_climate,"C:/Users/melmart/Documents/Data/LA_Tower/LA2_Hydro_Climate_Merge.csv",
#         row.names(FALSE))

#write.csv(la2_hydro_climate,"C:/Users/melmart/Documents/Data/LA_Tower/LA2_Hydro_Climate_Merge_2.csv",
#                   row.names=FALSE)
library(writexl)
#write_xlsx(la2_hydro_climate, "C:/Users/melmart/Documents/Data/LA_Tower/LA2_Hydro_Climate_Merge.xlsx")

# LA3 Site - Climate
head(la3_climate)
str(la3_climate)

la3_climate<-la3_climate %>% filter(Date > "2018-12-31*")

la3_climate$Year<- format(la3_climate$Date, "%Y")
la3_climate$Month<- format(la3_climate$Date, "%m")
la3_climate$Day<- format(la3_climate$Date, "%d")

la3_hydro$TIMESTAMP_START<- la3_hydro$TIMESTAMP_START %/% 100

la3_climate$TIMESTAMP_START<- as.numeric(paste0(la3_climate$Year,
                                                la3_climate$Month,la3_climate$Day))

la3_hydro_climate <- merge(la3_hydro,la3_climate %>% select(-c(Year,Month,Day)),by = "TIMESTAMP_START",all=TRUE)

names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.y"] <- "WS_Grid"

#st_write(la3_hydro_climate,"C:/Users/melmart/Documents/Data/LA_Tower/LA3_Hydro_Climate_Merge.csv",
#         row.names(FALSE))
