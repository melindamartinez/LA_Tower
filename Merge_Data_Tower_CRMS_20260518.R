library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(zoo)

setwd("~/GitHub/LA_Tower")

# Increase the penalty for scientific notation
options(scipen = 999)

###### CRMS Data #####
# CRMS 3166 - Freshwater Site (US-LA2)
hydro_3166<-read.csv("Data/CRMS Data/CRMS_3166_Hydro_Hourly/CRMS_3166_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)
# CHeck column names
head(hydro_3166)
str(hydro_3166)

# There was an issue with the degree symbol so convert to UTF-8
colnames(hydro_3166) <- iconv(colnames(hydro_3166),
                              from = "latin1",
                              to = "UTF-8")
# Remove parentheses from column names
names(hydro_3166) <- gsub("\\s*\\([^\\)]+\\)", "", names(hydro_3166))

# Combine Date and Time Columns
hydro_3166$DateTime<-paste(hydro_3166$Date,hydro_3166$Time, sep=" ")
# Format new Date Time column
hydro_3166$DateTime<-as.POSIXct(hydro_3166$DateTime,format= "%m/%d/%Y %H:%M:%S")

# CRMS 0224 - Salt Marsh Site (US-LA3)
hydro_0224<-read.csv("Data/CRMS Data/CRMS_0224_Hydro_Hourly/CRMS_0224_Hydro_Hourly.csv",
                     check.names = F, stringsAsFactors = TRUE)

# Check column names
head(hydro_0224)
str(hydro_0224)

# There was an issue with the degree symbol so convert to UTF-8
colnames(hydro_0224) <- iconv(colnames(hydro_0224),
                              from = "latin1",
                              to = "UTF-8")
# Remove parentheses from column names
names(hydro_0224) <- gsub("\\s*\\([^\\)]+\\)", "", names(hydro_0224))

hydro_0224$DateTime<-paste(hydro_0224$Date,hydro_0224$Time, sep=" ")
hydro_0224$DateTime<-as.POSIXct(hydro_0224$DateTime,format= "%m/%d/%Y %H:%M:%S")
#hydro_0224 %>% filter(DateTime > "2024-02-28*" & DateTime < "2024-03-01*")

##### AmeriFlux Data #####

la2_amf<-read.csv("Data/AMF_US-LA2_BASE-BADM_4-5/AMF_US-LA2_BASE_HH_4-5.csv",
                      skip = 2, header = TRUE)
la3_amf<-read.csv("Data/AMF_US-LA3_BASE-BADM_2-5/AMF_US-LA3_BASE_HH_2-5.csv",
                      skip = 2, header = TRUE)

# Checking colnames
names(la2_amf)
names(la3_amf)
# Need to remove the "_1_1_1" from LA3 names
names(la3_amf) <- gsub("_1_1_1$", "", names(la3_amf))


# Editing dataframe
la2_amf$DateTime<-ymd_hm(la2_amf$TIMESTAMP_START)
la3_amf$DateTime<-ymd_hm(la3_amf$TIMESTAMP_START)

# Checking Data available
ggplot(la2_amf %>% filter(FC != -9999 & DateTime > "2020-01-01"), aes(x=DateTime, y=FC)) + geom_point()
ggplot(la3_amf %>% filter(FC != -9999), aes(x=DateTime, y=FC)) + geom_point()

##### Merging Tower and CRMS Hydro Data #####
# LA2 Site - CRMS 3166
head(hydro_3166)
colnames(hydro_3166)

# Find column numbers that start with "Adjusted"
adjusted_columns <-  select(hydro_3166, starts_with("Adjusted")) %>% names()

# checking range of dates between two datasets
range(la2_amf$DateTime)
range(hydro_3166$DateTime,na.rm = TRUE)

# Selecting only adjusted columns from hydo_3166 and dates after the start of la2_amf
hydro_3166_select_H01<-hydro_3166 %>% filter(`Station ID` == "CRMS3166-H01" & DateTime > "2011-01-01*") %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns))

range(hydro_3166_select_H01$DateTime,na.rm = TRUE)

# Checking Time Zones to make sure they match
attr(la2_amf$DateTime, "tzone")
attr(hydro_3166_select_H01$DateTime, "tzone")

# Forcing to be the same timezone UTC
la2_amf$DateTime <- force_tz(la2_amf$DateTime, tzone = "UTC")
hydro_3166_select_H01$DateTime <- force_tz(
  hydro_3166_select_H01$DateTime,
  tzone = "UTC"
)

# Merging the two datasets
la2_hydro<-left_join(la2_amf,hydro_3166_select_H01,by="DateTime")

# Checking correlations with quick plot
plot(FCH4~`Adjusted Water Elevation to Datum`, la2_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Elevation to Datum`, la2_hydro %>% filter(FC != -9999))

# LA3 Site - CRMS 0224
head(hydro_0224)
colnames(hydro_0224)
unique(hydro_0224$`Station ID`)
# Find column numbers that start with "Adjusted"
adjusted_columns_224 <-  select(hydro_0224, starts_with("Adjusted")) %>% names()

range(la3_amf$DateTime)
range(hydro_0224$DateTime,na.rm = TRUE)

# Checking Time Zones to make sure they match
attr(la3_amf$DateTime, "tzone")
attr(hydro_0224$DateTime, "tzone")

# Forcing to be the same timezone UTC
la3_amf$DateTime <- force_tz(la3_amf$DateTime, tzone = "UTC")
hydro_0224$DateTime <- force_tz(
  hydro_0224$DateTime,
  tzone = "UTC"
)

#hydro_0224_select_H01<-hydro_0224 %>% filter(DateTime >= as.POSIXct("2019-01-01 00:00:00", tz = "UTC")) %>% 
#  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns_224))
hydro_0224_select_H01<-hydro_0224 %>% filter(DateTime >= as.POSIXct("2011-01-01 00:00:00", tz = "UTC")) %>% 
  select(`Station ID`, DateTime, `Sensor Environment`, all_of(adjusted_columns_224))

range(hydro_0224_select_H01$DateTime,na.rm = TRUE)

# Merging the two datasets
la3_hydro<-left_join(la3_amf,hydro_0224_select_H01,by="DateTime")
head(la3_hydro)
colnames(la3_hydro)

# Checking the merge
head(la3_amf %>% select(DateTime,FCH4, FC))
head(hydro_0224_select_H01 %>% select(DateTime,`Adjusted Water Level`))
head(la3_hydro %>% select(DateTime,`Adjusted Water Level`,FC,FCH4))

# Checking relationships with simple plot
plot(FCH4~`Adjusted Water Level`, la3_hydro %>% filter(FCH4 != -9999))
plot(FC~`Adjusted Water Level`, la3_hydro %>% filter(FC != -9999))


#### Adding GridMet Climate Data #####
la2_climate<-read_excel("Data/GridMet_LA2_All.xlsx",sheet = "Data")
la3_climate<-read_excel("Data/GridMet_LA3_All.xlsx",sheet = "Data")

# LA2 Site - Climate
head(la2_climate)
str(la2_climate)

la2_climate<-la2_climate %>% filter(Date > "2010-12-31")
head(la2_climate %>% select(Date,Precip))
head(la2_hydro %>% select(DateTime,FC,FCH4, `Adjusted Water Level`))


# Merge and fill in climate data for ever half hour
la2_hydro_climate <- la2_hydro %>%
  mutate(Date = as.Date(DateTime)) %>%
  left_join(
    la2_climate %>%
      mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>%
  select(-Date)

# Checking merged dataset
head(la2_hydro_climate %>% select(DateTime, Precip, `Adjusted Water Level`,FCH4))
names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la2_hydro_climate)[names(la2_hydro_climate) == "WS.y"] <- "WS_Grid"
str(la2_hydro_climate)

# LA3 Site - Climate
head(la3_climate)
str(la3_climate)

la3_climate<-la3_climate %>% filter(Date > "2018-12-31*")

# Merge and fill in climate data for ever half hour
la3_hydro_climate <- la3_hydro %>%
  mutate(Date = as.Date(DateTime)) %>%
  left_join(
    la3_climate %>%
      mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>%
  select(-Date)
head(la3_hydro_climate)


names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.x"] <- "WS_Eddy"
names(la3_hydro_climate)[names(la3_hydro_climate) == "WS.y"] <- "WS_Grid"

##### Correlation Plots
names(la3_hydro_climate)

la3_hydro_climate[la3_hydro_climate <= -9999.00] <- NA
la2_hydro_climate[la2_hydro_climate <= -9999.00] <- NA
range(la3_hydro_climate$TA,na.rm=TRUE)
str(la2_hydro_climate)

head(la3_hydro_climate)
df_la3<- la3_hydro_climate

# Read in DayLight_Hours R script to get "daylight_all" dataframe
source("DayLight_Hours.R")

df_la3$DateTime <- as.POSIXct(df_la3$DateTime, tz = "America/Chicago")
daylight_all$Rise <- as.POSIXct(daylight_all$Rise, tz = "America/Chicago")
daylight_all$Set  <- as.POSIXct(daylight_all$Set,  tz = "America/Chicago")

df_la3 <- df_la3 %>%
  mutate(Date = as.Date(DateTime))

daylight_all <- daylight_all %>%
  mutate(Date = as.Date(Rise))

df_daylight_only <- df_la3 %>%
  left_join(daylight_all, by = "Date") %>%
  filter(DateTime >= Rise & DateTime <= Set)

sw_red_la3 <- df_daylight_only %>%
  group_by(Date) %>%
  summarise(
    Down_SW_Rad_EC = ifelse(
      mean(!is.na(SW_IN)) >= 0.7,
      mean(SW_IN, na.rm = TRUE),
      NA_real_
    ),
    Down_SW_Rad_Grid = mean(Down_SW_Rad, na.rm = TRUE),
    PAR = ifelse(
      mean(!is.na(PPFD_IN)) >= 0.7,
      mean(PPFD_IN, na.rm = TRUE),
      NA_real_
    )
  )

# Plot data togather

ggplot(data=sw_red_la3) + geom_line(aes(x=Date, y= Down_SW_Rad_Grid, color="GridMet"))+
  geom_line(aes(x=Date, y= Down_SW_Rad_EC, color="EC"))

# View Correlations
model <- lm(Down_SW_Rad_Grid~Down_SW_Rad_EC,sw_red_la3)
summary(model)

eq <- paste0(
  "y = ",
  round(coef(model)[2], 3),
  "x + ",
  round(coef(model)[1], 3)
)

r2 <- paste0(
  "R² = ",
  round(summary(model)$r.squared, 3)
)

ggplot(sw_red_la3, aes(x = Down_SW_Rad_Grid, y = Down_SW_Rad_EC)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste(eq, r2, sep = "\n"),
           hjust = 1.1, vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red",size=1) +
  theme_minimal()



#modis_sw<-read.csv("C:/Users/melmart/Documents/Data/LP_DAAC/Extracted Data/MCD18A1_GMT1200_DSR_CRMS_224.csv")
#modis_sw$Date<-as.Date(modis_sw$Date, format = "%m/%d/%Y")
#str(modis_sw)
#
#sw_red_la3<-left_join(modis_sw,sw_red_la3, by="Date")
#ggplot(data=sw_red_la3) + geom_line(aes(x=Date, y= Down_SW_Rad_Grid, color="GridMet"))+
#  geom_line(aes(x=Date,y=GMT_1200_DSR, color="MODIS"))+
#  geom_line(aes(x=Date, y= Down_SW_Rad_EC, color="EC")) 



#### Temperature Correlations
df_la3 <- la3_hydro_climate %>%
  mutate(Date = as.Date(DateTime))
df_la2 <- la2_hydro_climate %>%
  mutate(Date = as.Date(DateTime))

temp_la3<- df_la3 %>% group_by(Date) %>% summarise(Mean_Temp_EC = mean(TA, na.rm=TRUE), 
                                        Max_Temp_EC = max(TA, na.rm=TRUE),
                                        Min_Temp_EC = min(TA,na.rm=TRUE),
                                        Mean_Temp_Grid = mean(Mean_Temp),
                                        Max_Temp_Grid = max(Max_Temp),
                                        Min_Temp_Grid = min(Min_Temp))
temp_la2<- df_la3 %>% group_by(Date) %>% summarise(Mean_Temp_EC = mean(TA, na.rm=TRUE), 
                                        Max_Temp_EC = max(TA, na.rm=TRUE),
                                        Min_Temp_EC = min(TA,na.rm=TRUE),
                                        Mean_Temp_Grid = mean(Mean_Temp),
                                        Max_Temp_Grid = max(Max_Temp),
                                        Min_Temp_Grid = min(Min_Temp))
plot(Min_Temp_EC~Min_Temp_Grid,temp_la3)


compare_two_vars <- function(data, x, y) {
  
  # -------------------------
  # 1. Build formula + model
  # -------------------------
  # keep only valid rows
  data <- data %>%
    filter(
      is.finite(.data[[x]]),
      is.finite(.data[[y]])
    )
  
  
  form <- as.formula(paste(y, "~", x))
  model <- lm(form, data = data)
  
  # -------------------------
  # 2. Equation + R²
  # -------------------------
  eq <- paste0(
    "y = ",
    round(coef(model)[2], 3),
    "x + ",
    round(coef(model)[1], 3)
  )
  
  r2 <- paste0(
    "R² = ",
    round(summary(model)$r.squared, 3)
  )
  
  # -------------------------
  # 3. Plot
  # -------------------------
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "red", size = 1) +
    annotate(
      "text",
      x = Inf, y = -Inf,
      label = paste(eq, r2, sep = "\n"),
      hjust = 1.1, vjust = -0.5
    ) +
    theme_minimal()
  
  # -------------------------
  # 4. Return everything
  # -------------------------
  list(
    model = model,
    summary = summary(model),
    plot = p
  )
}

result <- compare_two_vars(
  data = temp_la2,
  x = "Mean_Temp_EC",
  y = "Mean_Temp_Grid"
)

result$plot
names(df_la3)
precip_la3<-df_la3 %>% group_by(Date) %>% summarise(Precip_EC= sum(P_RAIN,na.rm=TRUE),
                                        Precip_Grid = sum(Precip)/100)
precip_la2<-df_la2 %>% group_by(Date) %>% summarise(Precip_EC= sum(P_RAIN,na.rm=TRUE),
                                        Precip_Grid = sum(Precip)/100)

result <- compare_two_vars(
  data = precip_la3,
  x = "Precip_EC",
  y = "Precip_Grid"
)

result$plot


water_la3<-df_la3 %>% group_by(Date) %>% summarise(WL=mean(`Adjusted Water Level`,na.rm=TRUE),
                                                   WT=mean(`Adjusted Water Temperature`,na.rm=TRUE),
                                                   Salinity=mean(`Adjusted Salinity`,na.rm=TRUE))

water_la3_filled <- water_la3 %>%
  mutate(
    WL_missing = is.na(WL),
    WT_missing = is.na(WT),
    Sal_missing = is.na(Salinity),
    WL = na.approx(WL, na.rm = FALSE),
    WT = na.approx(WT, na.rm = FALSE),
    Salinity = na.approx(Salinity, na.rm = FALSE)
  )


shade_df <- water_la3_filled %>%
  select(Date, WL_missing)

shade_df$grp <- data.table::rleid(shade_df$WL_missing)

shade_df <- shade_df %>%
  filter(WL_missing) %>%
  group_by(grp) %>%
  summarise(
    xmin = min(Date),
    xmax = max(Date),
    .groups = "drop"
  )

# Plot to see the difference
ggplot() +
  
  geom_rect(
    data = shade_df,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "blue",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    data = water_la3_filled,
    aes(Date, Salinity),
    color = "red",
    linewidth = 0.2
  ) +
  
  geom_line(
    data = water_la3,
    aes(Date, Salinity),
    color = "black",
    linewidth = 0.2
  ) +
  
  labs(
    y = "Water Level",
    title = "Interpolated periods highlighted"
  ) +
  
  theme_minimal()


water_la2<-df_la2 %>% group_by(Date) %>% summarise(WL=mean(`Adjusted Water Level`,na.rm=TRUE),
                                                   WT=mean(`Adjusted Water Temperature`,na.rm=TRUE),
                                                   Salinity=mean(`Adjusted Salinity`,na.rm=TRUE))


water_la2_filled <- water_la2 %>%
  mutate(
    WL_missing = is.na(WL),
    WT_missing = is.na(WT),
    Sal_missing = is.na(Salinity),
    WL = na.approx(WL, na.rm = FALSE),
    WT = na.approx(WT, na.rm = FALSE),
    Salinity = na.approx(Salinity, na.rm = FALSE)
  )

shade_df <- water_la2_filled %>%
  select(Date, WL_missing)

shade_df$grp <- data.table::rleid(shade_df$WL_missing)

shade_df <- shade_df %>%
  filter(WL_missing) %>%
  group_by(grp) %>%
  summarise(
    xmin = min(Date),
    xmax = max(Date),
    .groups = "drop"
  )

# Plot to see the difference
ggplot() +
  
  geom_rect(
    data = shade_df,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "blue",
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    data = water_la2_filled,
    aes(Date, WL),
    color = "red",
    linewidth = 0.2
  ) +
  
  geom_line(
    data = water_la2,
    aes(Date, WL),
    color = "black",
    linewidth = 0.2
  ) +
  
  labs(
    y = "Water Level",
    title = "Interpolated periods highlighted"
  ) +
  
  theme_minimal()


##### Relative Humidity #####

# Load package
library(plantecophys)

la3_hum<-la3_hydro_climate %>%
  mutate(Date = as.Date(DateTime))

# Calculate VPD (kPa)
la3_hum$VPD_EC <- RHtoVPD(RH = la3_hum$RH, TdegC=la3_hum$TA)

la3_hum_daily<-la3_hum %>% group_by(Date) %>% summarise(TA_md=median(TA,na.rm=TRUE),
                                                        RH_md=median(RH, na.rm=TRUE),
                                                        VPD_EC_md=mean(VPD_EC,na.rm=TRUE),
                                                        VPD_Grid=mean(VPD))

# View results
ggplot() + geom_point(data=la3_hum_daily, aes(x=VPD_Grid, y=VPD_EC_md))

result <- compare_two_vars(
  data = la3_hum_daily,
  x = "VPD_Grid",
  y = "VPD_EC_md"
)

result$plot

##### Merge full datasets #####

merged_la3 <- list(
  sw_red_la3 %>% select(Date,Down_SW_Rad_Grid),
  temp_la3 %>% select(Date,Mean_Temp_Grid),
  precip_la3 %>% select(Date, Precip_Grid),
  water_la3_filled %>% select(Date,WL,WT,Salinity),
  df_la3 %>%group_by(Date)%>% summarise(VPD = mean(VPD)) %>% select(Date,VPD)
) %>%
  reduce(full_join, by = "Date")

names(merged_la3) <- c("Date","Down_SW_Rad_Grid","Mean_temp_Grid","Precip_Grid","WL","WT","Salinity","VPD_Grid")
getwd()
#write.csv(merged_la3,"Data/Daily_Filled_Variables_LA3.csv",row.names = FALSE)


merged_la2 <- list(
  sw_red_la3 %>% select(Date,Down_SW_Rad_Grid),
  temp_la2 %>% select(Date,Mean_Temp_Grid),
  precip_la2 %>% select(Date, Precip_Grid),
  water_la2_filled %>% select(Date,WL,WT,Salinity),
  df_la2 %>%group_by(Date)%>% summarise(VPD = mean(VPD)) %>% select(Date,VPD)
) %>%
  reduce(full_join, by = "Date") %>% filter(Date >= "2019-01-01")

names(merged_la2) <- c("Date","Down_SW_Rad_Grid","Mean_temp_Grid","Precip_Grid","WL","WT","Salinity","VPD_Grid")

#write.csv(merged_la2,"Data/Daily_Filled_Variables_LA2.csv",row.names = FALSE)

