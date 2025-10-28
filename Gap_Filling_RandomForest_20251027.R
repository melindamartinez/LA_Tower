library(dplyr)
library(randomForest)
library(missForest)
library(ggplot2)
library(lubridate)
options(max.print = 10000)

setwd("C:/Users/melmart/Documents/Data/LA_Tower")

la2_data<-read.csv("LA2_Hydro_Climate_Merge_2.csv")


la2_reduced<-la2_data %>% dplyr::select(-contains(c("SSITC_TEST","MIXING_RATIO")),-c(TIMESTAMP_START,TIMESTAMP_END,DateTime.y,
                                               Station.ID,Date,Year,Month,Day,Time,
                                               Sensor.Environment))

names(la2_reduced)
names(la2_reduced)[names(la2_reduced) == "DateTime.x"] <- "DateTime"
names(la2_reduced) <- gsub("^Adjusted.[ ]*", "", names(la2_reduced))

str(la2_reduced)
la2_reduced$DateTime<-as.POSIXct(la2_reduced$DateTime,format= "%Y-%m-%d %H:%M:%S",tz="UTC")

full_seq <- seq(from = as.POSIXct("2011-01-01 00:00:00 UTC", format= "%Y-%m-%d %H:%M:%S",tz="UTC" ),
                to   = as.POSIXct("2024-03-01 00:00:00 UTC", format= "%Y-%m-%d %H:%M:%S",tz="UTC" ),
                by   = "30 min")

tail(sort(la2_reduced[1:230765,"DateTime"]))
length(la2_reduced[1:230700,"DateTime"])
length(full_seq)
tail(full_seq)

setdiff(head(full_seq, 50), la2_reduced[1:231000,"DateTime"])  # look at the earliest missing times
setdiff(tail(full_seq, 50), la2_reduced$DateTime)  # look at the latest ones


missing_times <- setdiff(sort(full_seq), as.POSIXct(sort(la2_reduced[1:100000,"DateTime"]),format= "%Y-%m-%d %H:%M:%S",tz="UTC" ))
missing_times

length(as.POSIXct(sort(la2_reduced[1:100000,"DateTime"]),format= "%Y-%m-%d %H:%M:%S",tz="UTC" ))
length(full_seq)

#la2_reduced<-la2_reduced[, c("DateTime", setdiff(names(la2_reduced), "DateTime"))]
#la2_reduced[,2:length(la2_reduced)] <- lapply(la2_reduced[,2:length(la2_reduced)], as.numeric)
#la2_reduced <- la2_reduced[order(la2_reduced$DateTime), ]
la2_reduced[la2_reduced <= -9999] <- NA

##### CH4 Gap Filling #####
la2_reduced_ch4<-la2_reduced %>% filter (DateTime > "2021-01-01*") %>% select(DateTime,Water.Level,FCH4,
                                                                       Water.Temperature,
                                                                       Precip,Mean_Temp,Min_Temp,Max_Temp,
                                                                       Sp_Hum,Down_SW_Rad,VPD,WS_Grid,PWD)

ggplot(la2_reduced_ch4, aes(x = DateTime, y = FCH4)) + geom_point()
ggplot(la2_reduced, aes(x = DateTime, y = FC)) + geom_point()
range(la2_reduced$Mean_Temp,na.rm=TRUE)

colnames(la2_reduced)

na_gaps <- la2_reduced %>% filter(DateTime > "2021-01-01") %>%
  mutate(
    is_na = is.na(FCH4),
    grp = cumsum(!is_na)  # increment group when value is not NA
  ) %>%
  group_by(grp) %>%
  summarise(
    gap_start = min(DateTime[is_na]),
    gap_end   = max(DateTime[is_na]),
    gap_length = sum(is_na),
    .groups = "drop"
  ) %>%
  filter(!is.na(gap_start))  # keep only NA groups

short_gaps<- na_gaps %>% filter(gap_length > 24)

la2_filtered <- la2_reduced %>%
  filter(DateTime > "2021-01-01") %>%
  filter(!Reduce(`|`,
                 Map(function(start, end) DateTime >= start & DateTime <= end,
                     short_gaps$gap_start, short_gaps$gap_end)))

ggplot(la2_filtered, aes(x = DateTime, y = FCH4)) + geom_point()


# Step 1: Filter out complete cases for FCH4
df_complete <- la2_filtered %>% filter(!is.na(FCH4))
head(df_complete)
# Step 2: Split into 70% train / 30% test
set.seed(42)  # for reproducibility
train_index <- sample(nrow(df_complete), 0.7 * nrow(df_complete))

train_data <- df_complete[train_index, ]
test_data  <- df_complete[-train_index, ]
length(unique(train_data$FCH4))
str(train_data$FCH4)
str(train_data)

# Step 3: Train the Random Forest model
# Exclude datetime if it's not used
model <- randomForest(
  FCH4 ~ Water.Level + Mean_Temp + VPD + Precip + Water.Temperature + Down_SW_Rad +Sp_Hum + Min_Temp,
  data = train_data %>% select(-DateTime),  # exclude datetime if needed
  na.action = na.omit,
  importance = TRUE
)

# Step 4: Validate on the 30% test set
test_predictors <- test_data %>% select(-FCH4, -DateTime)
test_actuals <- test_data$FCH4
range(test_data$FCH4)
predicted_test <- predict(model, newdata = test_predictors)

# Evaluate performance (e.g., RMSE)
rmse <- sqrt(mean((predicted_test - test_actuals)^2,na.rm=TRUE))
cat("Validation RMSE:", rmse, "\n")

# Filter rows where FCH4 is missing 
df_missing_clean <- la2_filtered %>% filter(is.na(FCH4))

head(df_missing_clean)

# Apply same predictors as model
df_missing_clean <- la2_reduced %>% 
  select(-DateTime)

# Predict missing FCH4 values
fch4_predicted <- predict(model, newdata = df_missing_clean)
head(fch4_predicted)

# Predict now
fch4_predicted <- predict(model, newdata = df_missing_clean)


# Add predictions back to original data
la2_filtered$FCH4_filled <- la2_filtered$FCH4
la2_filtered$FCH4_filled[is.na(la2_filtered$FCH4)] <- fch4_predicted
length(la2_filtered$FCH4_filled[is.na(la2_filtered$FCH4)])
length(fch4_predicted)

# Plotting
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = la2_filtered, aes(x = DateTime, y = FCH4, color = "Original"), na.rm = FALSE) +
  # Filled data only where FCH4 is NA (red), NA breaks the line
  geom_line(
    data = la2_filtered %>% filter(is.na(FCH4)),
    aes(x = DateTime, y = FCH4_filled, color = "Filled"),
    na.rm = FALSE
  ) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red")) +
  # X axis with major ticks yearly, minor ticks monthly
  scale_x_datetime(
    date_breaks = "1 months",       # major tick every year
    date_labels = "%b",           # show year labels
    limits = as.POSIXct(c("2021-05-01", "2022-12-31"))
  ) +
  # Shaded rectangles for large gaps
  geom_rect(
    data = short_gaps,
    aes(
      xmin = gap_start,
      xmax = gap_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "white", alpha = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "Actual vs Filled FCH4 Over Time",
    x = "Date",
    y = "FCH4 (µmol/m²/s)",
    color = "Legend"
  ) +
  theme_bw()

summary(la2_filtered$FCH4_filled)

##### CO2 Gap Filling #####
colnames(la2_reduced)
la2_reduced_co2<-la2_reduced %>% filter (DateTime > "2021-01-01*") %>% dplyr::select(DateTime,Water.Level,FC,
                                                                          Water.Temperature,
                                                                          Precip,Mean_Temp,Min_Temp,Max_Temp,
                                                                          Sp_Hum,Down_SW_Rad,VPD,WS_Grid,PWD, LW_IN, SW_IN, PPFD_IN,USTAR)

ggplot(la2_reduced_co2, aes(x = DateTime, y = FC)) + geom_point()
colnames(la2_reduced_co2)

la2_filtered <- la2_reduced_co2 %>%
  filter(DateTime > "2021-06-01" & DateTime < "2021-10-31") %>%
  filter(!Reduce(`|`,
                 Map(function(start, end) DateTime >= start & DateTime <= end,
                     short_gaps$gap_start, short_gaps$gap_end)))

ggplot(la2_filtered, aes(x = DateTime, y = FC)) + geom_line()

na_gaps <- la2_filtered %>%
  mutate(
    is_na = is.na(FC),
    grp = cumsum(!is_na)  # increment group when value is not NA
  ) %>%
  group_by(grp) %>%
  summarise(
    gap_start = min(DateTime[is_na]),
    gap_end   = max(DateTime[is_na]),
    gap_length = sum(is_na),
    .groups = "drop"
  ) %>%
  filter(!is.na(gap_start))  # keep only NA groups

short_gaps<- na_gaps %>% filter(gap_length > 48)

#### 7 day moving average #####
library(data.table)
#as.POSIXct(la2_data$DateTime.x,format= "%Y-%m-%d %H:%M:%S",tz="UTC")

la2_reduced_ch4<-la2_reduced %>% filter (DateTime > "2021-01-01*") %>% select(DateTime,Water.Level,FCH4,
                                                                              Water.Temperature,
                                                                              Precip,Mean_Temp,Min_Temp,Max_Temp,
                                                                              Sp_Hum,Down_SW_Rad,VPD,WS_Grid,PWD)

la2_reduced_co2<-la2_reduced %>% filter (DateTime > "2021-01-01*") %>% select(DateTime,Water.Level,FC,
                                                                              Water.Temperature,
                                                                              Precip,Mean_Temp,Min_Temp,Max_Temp,
                                                                              Sp_Hum,Down_SW_Rad,VPD,WS_Grid,PWD)



ggplot(la2_reduced_ch4, aes(x = DateTime, y = FCH4)) + geom_point()
ggplot(la2_reduced_co2, aes(x = DateTime, y = FC)) + geom_point()

la2_reduced[1:40,]
# --- Generate full expected half-hour sequence ---
full_seq <- seq(from = as.POSIXct("2021-01-01 06:30:00 UTC", format= "%Y-%m-%d %H:%M:%S",tz="UTC" ),
                to   = max(la2_reduced_co2$DateTime, na.rm = TRUE),
                by   = "30 min")
length(la2_reduced_co2$DateTime)
length(full_seq)
# --- Find missing timestamps ---
missing_times <- setdiff(full_seq, la2_reduced_co2$DateTime)
length(missing_times)

la2_reduced_co2$DateTime_Full<-full_seq

gap_fill_7day_slot_mean <- function(dt, ts_col = "value", time_col = "datetime",
                                    min_obs = 3) {
  # dt: data.frame or data.table containing POSIXct time_col and value column
  # min_obs: minimum non-NA observations in the 7-day window to accept fill
  
  setDT(dt)
  stopifnot(inherits(dt[[time_col]], "POSIXt"))
  dt[, date := as.Date(get(time_col))]
  dt[, slot := (as.integer(format(get(time_col), "%H")) * 2) +
       (as.integer(format(get(time_col), "%M")) / 30) + 1] 
  # slot in 1..48 (1 = 00:00-00:30, 48 = 23:30-00:00)
  # ensure integer
  dt[, slot := as.integer(slot)]
  
  # pivot to wide: rows = date, cols = slot
  wide <- dcast(dt, date ~ slot, value.var = ts_col)
  date_vec <- wide$date
  mat <- as.matrix(wide[, -1, with = FALSE])  # rows = day, cols = slot (48)
  
  n_days <- nrow(mat)
  n_slots <- ncol(mat)  # should be 48
  
  # result matrix with same dims: for each (day,slot) compute mean over +/-3 days
  res_mat <- matrix(NA_real_, nrow = n_days, ncol = n_slots)
  
  for (i in seq_len(n_days)) {
    lo <- max(1, i - 3)   # 7-day window centered; days i-3 .. i+3
    hi <- min(n_days, i + 3)
    block <- mat[lo:hi, , drop = FALSE]  # submatrix (days in window x slots)
    # compute column means ignoring NA; we'll check min_obs
    non_na_counts <- colSums(!is.na(block))
    col_means <- colMeans(block, na.rm = TRUE)
    # apply min_obs threshold
    col_means[non_na_counts < min_obs] <- NA_real_
    res_mat[i, ] <- col_means
  }
  
  # Map back: where original is NA and res_mat has a value, fill it
  filled_mat <- mat
  nas_idx <- which(is.na(mat), arr.ind = TRUE)
  for (k in seq_len(nrow(nas_idx))) {
    r <- nas_idx[k, 1]; ccol <- nas_idx[k, 2]
    val <- res_mat[r, ccol]
    if (!is.na(val)) filled_mat[r, ccol] <- val
  }
  
  # convert wide back to long
  filled_wide <- data.table(date = date_vec, as.data.table(filled_mat))
  long <- melt(filled_wide, id.vars = "date", variable.name = "slot", value.name = ts_col)
  long[, slot := as.integer(slot)]
  # rebuild datetime
  long[, datetime := as.POSIXct(date) + (slot - 1) * 30 * 60]
  # join back to original times to preserve original timestamps ordering
  out <- merge(dt[, .(datetime = get(time_col), orig = get(ts_col))], 
               long[, .(datetime, filled = get(ts_col))], by = "datetime", all.x = TRUE, sort = FALSE)
  out[, final := fifelse(is.na(orig), filled, orig)]
  out[, .(datetime, orig, final)]
}

new_co2<-gap_fill_7day_slot_mean(la2_filtered,ts_col = "FC",time_col = "DateTime",min_obs = 3)

# Plotting
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = new_co2, aes(x = datetime, y = orig, color = "Original"), na.rm = FALSE) +
  # Filled data only where FC is NA (red), NA breaks the line
  geom_line(
    data = new_co2 %>% filter(is.na(orig)),
    aes(x = datetime, y = final, color = "Filled"), na.rm = FALSE) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red")) +
  # X axis with major ticks yearly, minor ticks monthly
  scale_x_datetime(
    date_breaks = "1 week",       # major tick every year
    date_labels = "%b-%d",           # show year labels
    limits = as.POSIXct(c("2021-06-15", "2021-07-15"))
  )+
# Shaded rectangles for large gaps
geom_rect(
  data = short_gaps,
  aes(
    xmin = gap_start,
    xmax = gap_end,
    ymin = -Inf,
    ymax = Inf
  ),
  fill = "grey", alpha = 0.3, inherit.aes = FALSE
) +
  labs(
    title = "Actual vs Filled FC Over Time",
    x = "Date",
    y = "FC (µmol/m²/s)",
    color = "Legend"
  ) +
  theme_bw()

summary(la2_filtered$FC_filled)

##### Random Forest Model #####
# Step 1: Filter out complete cases for FC
df_complete <- la2_filtered %>% filter(!is.na(FC))
head(df_complete)
# Step 2: Split into 70% train / 30% test
set.seed(42)  # for reproducibility
train_index <- sample(nrow(df_complete), 0.7 * nrow(df_complete))

train_data <- df_complete[train_index, ]
test_data  <- df_complete[-train_index, ]
length(unique(train_data$FC))
str(train_data$FC)
str(train_data)

# Step 3: Train the Random Forest model
# Exclude datetime if it's not used
model <- randomForest(
  FC ~ Water.Level + Mean_Temp + VPD + Precip + Water.Temperature + Down_SW_Rad +Sp_Hum + Min_Temp,
  data = train_data %>% select(-DateTime),  # exclude datetime if needed
  na.action = na.omit,
  importance = TRUE
)

# Step 4: Validate on the 30% test set
test_predictors <- test_data %>% select(-FC, -DateTime)
test_actuals <- test_data$FC
range(test_data$FC)
predicted_test <- predict(model, newdata = test_predictors)

# Evaluate performance (e.g., RMSE)
rmse <- sqrt(mean((predicted_test - test_actuals)^2,na.rm=TRUE))
cat("Validation RMSE:", rmse, "\n")

# Filter rows where FC is missing 
df_missing_clean <- la2_filtered %>% filter(is.na(FC))

head(df_missing_clean)

# Apply same predictors as model
df_missing_clean <- la2_reduced %>% 
  select(-DateTime)

# Predict missing FC values
FC_predicted <- predict(model, newdata = df_missing_clean)
head(FC_predicted)

# Predict now
FC_predicted <- predict(model, newdata = df_missing_clean)


# Add predictions back to original data
la2_filtered$FC_filled <- la2_filtered$FC
la2_filtered$FC_filled[is.na(la2_filtered$FC)] <- FC_predicted
length(la2_filtered$FC_filled[is.na(la2_filtered$FC)])
length(FC_predicted)

# Plotting
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = la2_filtered, aes(x = DateTime, y = FC, color = "Original"), na.rm = FALSE) +
  # Filled data only where FC is NA (red), NA breaks the line
  geom_line(
    data = la2_filtered %>% filter(is.na(FC)),
    aes(x = DateTime, y = FC_filled, color = "Filled"),
    na.rm = FALSE
  ) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red")) +
  # X axis with major ticks yearly, minor ticks monthly
  scale_x_datetime(
    date_breaks = "1 months",       # major tick every year
    date_labels = "%b",           # show year labels
    limits = as.POSIXct(c("2021-06-15", "2021-07-15"))
  ) +
  # Shaded rectangles for large gaps
  geom_rect(
    data = short_gaps,
    aes(
      xmin = gap_start,
      xmax = gap_end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "white", alpha = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "Actual vs Filled FC Over Time",
    x = "Date",
    y = "FC (µmol/m²/s)",
    color = "Legend"
  ) +
  theme_bw()

summary(la2_filtered$FC_filled)


