library(dplyr)
library(randomForest)
library(missForest)
library(ggplot2)
library(lubridate)
options(max.print = 10000)


##### CH4 Gap Filling #####
merged_la3 # This data frame comes from merging R script

flux_data<-read.csv("Data/US_LA3_mean_flux_clean_2019_2023.csv")

flux_data$Date<-ymd(flux_data$date)

flux_var_la3<-left_join(flux_data %>% select(-date),merged_la3,by = "Date") %>% filter(Date >= "2020-01-01")
flux_var_la3$DOY<-yday(flux_var_la3$Date)

colnames(flux_var_la3)
str(flux_var_la3)

ggplot(flux_var_la3, aes(x = Date, y = FCH4mean)) + geom_line() +  
  ylab(expression(n * mol ~ CH[4] ~ m^{-2} ~ day^{-1})) + theme_light() + ggtitle("EC Tower: LA3", subtitle = "Methane") # 3x6
ggplot(flux_var_la3, aes(x = Date, y = FCmean)) + geom_line() + 
  ylab(expression(mu * mol ~ CO[2] ~ m^{-2} ~ day^{-1})) + theme_light() + ggtitle("EC Tower: LA3", subtitle = "Carbon Dioxide") # 3x6




# Step 1: Filter out complete cases for FCH4
df_complete <- flux_var_la3 %>% filter(!is.na(FCH4mean))
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
  FCH4mean ~ .,
  data = train_data %>% select(-Date,-FCmean),  # exclude datetime if needed
  na.action = na.omit,
  importance = TRUE
)

# Step 4: Validate on the 30% test set
test_predictors <- test_data %>% select(-FCH4mean, -Date)
test_actuals <- test_data$FCH4mean
range(test_data$FCH4mean)
predicted_test <- predict(model, newdata = test_predictors)

# Evaluate performance (e.g., RMSE)
rmse <- sqrt(mean((predicted_test - test_actuals)^2,na.rm=TRUE))
cat("Validation RMSE:", rmse, "\n")

# R-squared
r2 <- cor(predicted_test, test_actuals, use = "complete.obs")^2

cat("Validation RMSE:", rmse, "\n")
cat("Validation R²:", r2, "\n")

# Filter rows where FCH4 is missing 
df_missing_clean <- flux_var_la3 %>% filter(is.na(FCH4mean)) %>% select(-Date, -FCmean)

head(df_missing_clean)

# Predict missing FCH4 values
fch4_predicted <- predict(model, newdata = df_missing_clean)
head(fch4_predicted)

# Predict now
fch4_predicted <- predict(model, newdata = df_missing_clean)

# Add predictions back to original data
flux_var_la3$FCH4_filled <- flux_var_la3$FCH4mean
flux_var_la3$FCH4_filled[is.na(flux_var_la3$FCH4mean)] <- fch4_predicted
length(flux_var_la3$FCH4_filled[is.na(flux_var_la3$FCH4mean)])
length(fch4_predicted)

# Plotting
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = flux_var_la3, aes(x = Date, y = FCH4mean, color = "Original"), na.rm = FALSE) +
  # Filled data only where FCH4 is NA (red), NA breaks the line
  geom_line(
    data = flux_var_la3 %>% filter(is.na(FCH4mean)),
    aes(x = Date, y = FCH4_filled, color = "Filled"),
    na.rm = FALSE
  ) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red")) +
  # X axis with major ticks yearly, minor ticks monthly
  scale_x_datetime(
    date_breaks = "6 months",       # major tick every year
    date_labels = "%Y-%b") +           # show year labels 
  annotate(
    "text",
    x = min(flux_var_la3$Date, na.rm = TRUE),
    y = max(flux_var_la3$FCH4_filled, na.rm = TRUE),
    hjust = 0,
    vjust = 1,
    size = 4,
    label = paste0(
      "RMSE = ", round(rmse, 2),
      "\nR² = ", round(r2, 2)
    )
  ) +
  labs(
    title = "Actual vs Filled Methane Over Time",
    x = "Date",
    y = expression(nmol~CH[4]~m^{-2}~s^{-1}),
    color = "Legend"
  ) +
  theme_light() # 3 x 9 landscape


##### CO2 Gap Filling #####
# Step 1: Filter out complete cases for FC
df_complete <- flux_var_la3 %>% filter(!is.na(FCmean))
head(df_complete)
# Step 2: Split into 70% train / 30% test
set.seed(42)  # for reproducibility
train_index <- sample(nrow(df_complete), 0.7 * nrow(df_complete))

train_data <- df_complete[train_index, ]
test_data  <- df_complete[-train_index, ]
length(unique(train_data$FCmean))
str(train_data$FCmean)
str(train_data)

# Step 3: Train the Random Forest model
# Exclude datetime if it's not used
model <- randomForest(
  FCmean ~ .,
  data = train_data %>% select(-c(Date,FCH4_filled,FCH4mean)),  # exclude datetime if needed
  na.action = na.omit,
  importance = TRUE
)

# Step 4: Validate on the 30% test set
test_predictors <- test_data %>% select(-c(Date,FCH4_filled,FCH4mean))
test_actuals <- test_data$FCmean
range(test_data$FCmean)
predicted_test <- predict(model, newdata = test_predictors)

# Evaluate performance (e.g., RMSE)
rmse <- sqrt(mean((predicted_test - test_actuals)^2,na.rm=TRUE))
# R-squared
r2 <- cor(predicted_test, test_actuals, use = "complete.obs")^2

cat("Validation RMSE:", rmse, "\n")
cat("Validation R²:", r2, "\n")

# Filter rows where FC is missing 
df_missing_clean <- flux_var_la3 %>% filter(is.na(FCmean)) %>% select(-c(Date,FCH4_filled,FCH4mean))

head(df_missing_clean)

# Predict missing FC values
FC_predicted <- predict(model, newdata = df_missing_clean)
head(FC_predicted)

# Predict now
FC_predicted <- predict(model, newdata = df_missing_clean)


# Add predictions back to original data
flux_var_la3$FC_filled <- flux_var_la3$FCmean
flux_var_la3$FC_filled[is.na(flux_var_la3$FCmean)] <- FC_predicted
length(flux_var_la3$FC_filled[is.na(flux_var_la3$FCmean)])
length(FC_predicted)

# Plotting
ggplot() +
  # Original data (black), NA breaks the line
  geom_line(data = flux_var_la3, aes(x = Date, y = FCmean, color = "Original"), na.rm = FALSE) +
  # Filled data only where FC is NA (red), NA breaks the line
  geom_line(
    data = flux_var_la3 %>% filter(is.na(FCmean)),
    aes(x = Date, y = FC_filled, color = "Filled"),
    na.rm = FALSE
  ) +
  scale_color_manual(values = c("Original" = "black", "Filled" = "red")) +
  # X axis with major ticks yearly, minor ticks monthly
  scale_x_datetime(
    date_breaks = "6 months",       # major tick every year
    date_labels = "%Y-%b") +           # show year labels 
  annotate(
    "text",
    x = min(flux_var_la3$Date, na.rm = TRUE),
    y = max(flux_var_la3$FC_filled, na.rm = TRUE),
    hjust = 0,
    vjust = 1,
    size = 4,
    label = paste0(
      "RMSE = ", round(rmse, 2),
      "\nR² = ", round(r2, 2)
    )
  ) +
  labs(
    title = "Actual vs Filled Carbon Dioxide Over Time",
    x = "Date",
    y = expression(mu*mol~CO[2]~m^{-2}~s^{-1}),
    color = "Legend"
  ) +
  theme_light() # 3 x 9 landscape

summary(flux_var_la3$FC_filled)

#write.csv(flux_var_la3,"Data/Daily_Gapfilled_LA3.csv", row.names = FALSE)
