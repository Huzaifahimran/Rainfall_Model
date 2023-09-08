#setwd("C:/Users/Sahar/OneDrive/Desktop/R")

####Libraries####
library(tidyverse)
library(zoo)
library(forecast)
library(MASS)
library(e1071)
library(earth)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(stats)
library(GGally)
library(stringr)
library(tidyr)
library(svglite)
library(ggfortify)
library(gridExtra)
library(purrr)



####Data Import####
cdr_57002<-read.csv("57002_cdr.csv",skip = 19)
gdf_57002<-read.csv("57002_gdf.csv",skip = 19)
cdr_57004<-read.csv("57004_cdr.csv",skip = 20)
gdf_57004<-read.csv("57004_gdf.csv",skip = 20)
cdr_57005<-read.csv("57005_cdr.csv",skip = 20)
gdf_57005<-read.csv("57005_gdf.csv",skip = 20)
cdr_57006<-read.csv("57006_cdr.csv",skip = 20)
gdf_57006<-read.csv("57006_gdf.csv",skip = 20)
cdr_57007<-read.csv("57007_cdr.csv",skip = 20)
gdf_57007<-read.csv("57007_gdf.csv",skip = 20)
cdr_57015<-read.csv("57015_cdr.csv",skip = 20)
gdf_57015<-read.csv("57015_gdf.csv",skip = 20)
cdr_57017<-read.csv("57017_cdr.csv",skip = 20)
gdf_57017<-read.csv("57017_gdf.csv",skip = 20)


####Data Cleaning####
#Kept Relevant Columns
cdr_57002<-cdr_57002[,c(1,2)]
gdf_57002<-gdf_57002[,c(1,2)]
cdr_57004<-cdr_57004[,c(1,2)]
gdf_57004<-gdf_57004[,c(1,2)]
cdr_57005<-cdr_57005[,c(1,2)]
gdf_57005<-gdf_57005[,c(1,2)]
cdr_57006<-cdr_57006[,c(1,2)]
gdf_57006<-gdf_57006[,c(1,2)]
cdr_57007<-cdr_57007[,c(1,2)]
gdf_57007<-gdf_57007[,c(1,2)]
cdr_57015<-cdr_57015[,c(1,2)]
gdf_57015<-gdf_57015[,c(1,2)]
cdr_57017<-cdr_57017[,c(1,2)]
gdf_57017<-gdf_57017[,c(1,2)]


#Renamed the Columns
names(cdr_57002)<-c("Date","Rainfall")
names(cdr_57004)<-c("Date","Rainfall")
names(cdr_57005)<-c("Date","Rainfall")
names(cdr_57006)<-c("Date","Rainfall")
names(cdr_57007)<-c("Date","Rainfall")
names(cdr_57015)<-c("Date","Rainfall")
names(cdr_57017)<-c("Date","Rainfall")

names(gdf_57002)<-c("Date","Flow")
names(gdf_57004)<-c("Date","Flow")
names(gdf_57005)<-c("Date","Flow")
names(gdf_57006)<-c("Date","Flow")
names(gdf_57007)<-c("Date","Flow")
names(gdf_57015)<-c("Date","Flow")
names(gdf_57017)<-c("Date","Flow")

#Formatted Date Variables Properly
cdr_57002$Date<-as.Date(cdr_57002$Date,format = "%Y-%m-%d")
cdr_57004$Date<-as.Date(cdr_57004$Date,format = "%Y-%m-%d")
cdr_57005$Date<-as.Date(cdr_57005$Date,format = "%Y-%m-%d")
cdr_57006$Date<-as.Date(cdr_57006$Date,format = "%Y-%m-%d")
cdr_57007$Date<-as.Date(cdr_57007$Date,format = "%Y-%m-%d")
cdr_57015$Date<-as.Date(cdr_57015$Date,format = "%Y-%m-%d")
cdr_57017$Date<-as.Date(cdr_57017$Date,format = "%Y-%m-%d")
gdf_57002$Date<-as.Date(gdf_57002$Date,format = "%Y-%m-%d")
gdf_57004$Date<-as.Date(gdf_57004$Date,format = "%Y-%m-%d")
gdf_57005$Date<-as.Date(gdf_57005$Date,format = "%Y-%m-%d")
gdf_57006$Date<-as.Date(gdf_57006$Date,format = "%Y-%m-%d")
gdf_57007$Date<-as.Date(gdf_57007$Date,format = "%Y-%m-%d")
gdf_57015$Date<-as.Date(gdf_57015$Date,format = "%Y-%m-%d")
gdf_57017$Date<-as.Date(gdf_57017$Date,format = "%Y-%m-%d")

# Determine the period from 1990 t0 2017 
cdr_gdf_57002<-full_join(cdr_57002,gdf_57002,by="Date")

# Convert the 'Date' column to Date format
cdr_gdf_57002$Date <- as.Date(cdr_gdf_57002$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57002 <- cdr_gdf_57002[cdr_gdf_57002$Date >= start_date & cdr_gdf_57002$Date <= end_date, ]

View(filtered_cdr_gdf_57002)


#Remove empty rows
G57002 <- filtered_cdr_gdf_57002[complete.cases(filtered_cdr_gdf_57002), ]

# Keep rows with Rainfall values and either non-empty Flow values or NA in the Flow column
G57002 <- filtered_cdr_gdf_57002[!is.na(filtered_cdr_gdf_57002$Rainfall) & (!is.na(filtered_cdr_gdf_57002$Flow) | is.na(filtered_cdr_gdf_57002$Flow)), ]


View(G57002)


# Determine the period from 1990 t0 2017 
cdr_gdf_57004<-full_join(cdr_57004,gdf_57004,by="Date")
# Convert the 'Date' column to Date format
cdr_gdf_57004$Date <- as.Date(cdr_gdf_57004$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57004 <- cdr_gdf_57004[cdr_gdf_57004$Date >= start_date & cdr_gdf_57004$Date <= end_date, ]

# Remove empty rows
G57004 <- filtered_cdr_gdf_57004[complete.cases(filtered_cdr_gdf_57004), ]







# Determine the period from 1990 t0 2017 
cdr_gdf_57005<-full_join(cdr_57005,gdf_57005,by="Date")
# Convert the 'Date' column to Date format
cdr_gdf_57005$Date <- as.Date(cdr_gdf_57005$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57005 <- cdr_gdf_57005[cdr_gdf_57005$Date >= start_date & cdr_gdf_57005$Date <= end_date, ]

# Remove empty rows
G57005 <- filtered_cdr_gdf_57005[complete.cases(filtered_cdr_gdf_57005), ]



# Determine the period from 1990 t0 2017 
cdr_gdf_57006<-full_join(cdr_57006,gdf_57006,by="Date")
# Convert the 'Date' column to Date format
cdr_gdf_57006$Date <- as.Date(cdr_gdf_57006$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57006 <- cdr_gdf_57006[cdr_gdf_57006$Date >= start_date & cdr_gdf_57006$Date <= end_date, ]

# Remove empty rows
G57006 <- filtered_cdr_gdf_57006[complete.cases(filtered_cdr_gdf_57006), ]




# Determine the period from 1990 t0 2017 
cdr_gdf_57007<-full_join(cdr_57007,gdf_57007,by="Date")
# Convert the 'Date' column to Date format
cdr_gdf_57007$Date <- as.Date(cdr_gdf_57007$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57007 <- cdr_gdf_57007[cdr_gdf_57007$Date >= start_date & cdr_gdf_57007$Date <= end_date, ]

# Remove empty rows
G57007 <- filtered_cdr_gdf_57007[complete.cases(filtered_cdr_gdf_57007), ]



# Determine the period from 1990 t0 2017 
cdr_gdf_57015<-full_join(cdr_57015,gdf_57015,by="Date")
# Convert the 'Date' column to Date format
cdr_gdf_57015$Date <- as.Date(cdr_gdf_57015$Date)

# Filter the dataframe for the desired time range
start_date <- as.Date("1990-01-01")
end_date <- as.Date("2017-12-31")
filtered_cdr_gdf_57015 <- cdr_gdf_57015[cdr_gdf_57015$Date >= start_date & cdr_gdf_57015$Date <= end_date, ]

# Remove empty rows
G57015 <- filtered_cdr_gdf_57015[complete.cases(filtered_cdr_gdf_57015), ]





# Create a list to store the tables
tables <- list(G57002, G57004, G57005, G57006, G57007, G57015)




# Add table names as a new column in each table
tables <- lapply(seq_along(tables), function(i) {
  table <- tables[[i]]
  table$Gauge <- paste0("Table_", i)
  return(table)
})

# Combine the tables into one
combined_table <- bind_rows(tables)

# View the combined table
#View(combined_table)


# Replace the table name with Guage name 
combined_table <- mutate(combined_table, Gauge = ifelse(Gauge == 'Table_1', 'G57002',
                                                        ifelse(Gauge == 'Table_2', 'G57004',
                                                               ifelse(Gauge == 'Table_3', 'G57005',
                                                                      ifelse(Gauge == 'Table_4', 'G57006',
                                                                             ifelse(Gauge == 'Table_5', 'G57007',
                                                                                    ifelse(Gauge == 'Table_6', 'G57015', Gauge)))))))




# Generate the "month" variable
daily_data <- combined_table %>%
  mutate(month = format(Date, "%m"))


#------------------------------------------------------************-----------------------------------------------------------
# Define the target gauges
target_gauges <- c("G57002", "G57004", "G57005", "G57006", "G57007", "G57015")

# Number of lagged values
lag_days <- 6

# Function to create lagged columns for a specific target gauge
create_lagged_columns <- function(target_gauge, lag_days) {
  target_data <- daily_data %>%
    filter(Gauge == target_gauge)
  
  for (lag in 1:lag_days) {
    flow_lag_col <- paste0("Flow_Lag", lag, "_", target_gauge)
    rainfall_lag_col <- paste0("Rainfall_Lag", lag, "_", target_gauge)
    
    target_data <- target_data %>%
      mutate(
        !!flow_lag_col := lag(Flow, lag),
        !!rainfall_lag_col := lag(Rainfall, lag)
      )
  }
  
  target_data <- target_data[, -(1:5)]
  return(target_data)
}

# Create a list to store the data frames for each target gauge
target_data_list <- lapply(target_gauges, create_lagged_columns, lag_days = lag_days)

# Combine all the data frames in the list using reduce from the purrr package

final_data <- reduce(target_data_list, bind_cols)




# Filter data for Gauge G57005
gauge_G57005_data <- daily_data %>%
  filter(Gauge == "G57005")

# Combine target_data and gauge_G57005_data using bind_cols
G57005 <- bind_cols( gauge_G57005_data, final_data)

# Remove rows with missing values
G57005 <- na.omit(G57005)

View(G57005)


##################################################################33
#### with seasonal intercept 

# Create dummy variables for each season (month in this example)
G57005_dummy <- G57005 %>%
  mutate(
    Month = factor(month(Date))  # Extract month from the Date column
  )

# Create dummy variables for each month using model.matrix
month_dummies <- model.matrix(~ Month - 1 , data = G57005_dummy)

# Combine the dummy variables with the G57005_dummy data
G57005 <- cbind(G57005_dummy, month_dummies)

View(G57005)

# Select the response and predictor columns for gauge G57005
response_column <- "Flow"


# Select the response and predictor columns for gauge G57005
response_predictor_columns <- c(response_column,
                                paste0("Flow_Lag", 1:lag_days, "_G57002"),
                                paste0("Flow_Lag", 1:lag_days, "_G57015"),
                                paste0("Flow_Lag", 1:lag_days, "_G57004"),
                                paste0("Flow_Lag", 1:lag_days, "_G57007"),
                                paste0("Flow_Lag", 1:lag_days, "_G57006"),
                                paste0("Flow_Lag", 1:lag_days, "_G57005"),            
                                paste0("Rainfall_Lag", 1:lag_days, "_G57002"),
                                paste0("Rainfall_Lag", 1:lag_days, "_G57015"),
                                paste0("Rainfall_Lag", 1:lag_days, "_G57004"),
                                paste0("Rainfall_Lag", 1:lag_days, "_G57007"),
                                paste0("Rainfall_Lag", 1:lag_days, "_G57006"),
                                paste0("Rainfall_Lag", 1:lag_days, "_G57005"))   


# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
n <- nrow(G57005)
test_size <- round(0.2 * n)  # 20% of the total data
test_indices <- (n - test_size + 1):n  # Select indices from the end for the test data
train_indices <- setdiff(1:n, test_indices)  # Exclude test indices to get training indices

train_data <- G57005[train_indices, ]
test_data <- G57005[test_indices, ]

# build lm model using all variables : 

lm1 <- lm(Flow ~ . , data = train_data[,response_predictor_columns])



summary(lm1)


checkresiduals(lm1)

par(mfrow=c(2,3))
plot(lm1, which=1:6)


# Generate the forecast using the linear regression model on the test data
lm1_forecast_values <- predict(lm1, newdata = test_data)

# Calculate accuracy metrics for the forecast
lm1_mae <- mean(abs(lm1_forecast_values - test_data$Flow))
lm1_rmse <- sqrt(mean((lm1_forecast_values - test_data$Flow)^2))
lm1_mape <- mean(abs((lm1_forecast_values - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_mape, "%\n")



# Create a data frame containing the test_data and forecast_values
lm1_scatter_data <- data.frame(Date = test_data$Date, 
                               Observed = test_data$Flow,
                               Forecasted = lm1_forecast_values)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow") +
  theme_minimal()

### using step function 
step(lm1)

# the model with chosen variables 

lm2 <- lm( Flow ~ Flow_Lag2_G57002 + Flow_Lag3_G57002 + Flow_Lag4_G57002 + 
             Flow_Lag6_G57002 + Flow_Lag1_G57015 + Flow_Lag3_G57015 + 
             Flow_Lag4_G57015 + Flow_Lag6_G57015 + Flow_Lag3_G57004 + 
             Flow_Lag6_G57004 + Flow_Lag1_G57007 + Flow_Lag2_G57007 + 
             Flow_Lag3_G57007 + Flow_Lag5_G57007 + Flow_Lag1_G57006 + 
             Flow_Lag3_G57006 + Flow_Lag4_G57006 + Flow_Lag5_G57006 + 
             Flow_Lag6_G57006 + Flow_Lag1_G57005 + Flow_Lag2_G57005 + 
             Flow_Lag3_G57005 + Flow_Lag4_G57005 + Flow_Lag5_G57005 + 
             Flow_Lag6_G57005 + Rainfall_Lag1_G57002 + Rainfall_Lag4_G57002 + 
             Rainfall_Lag5_G57002 + Rainfall_Lag6_G57002 + Rainfall_Lag1_G57015 + 
             Rainfall_Lag2_G57015 + Rainfall_Lag4_G57015 + Rainfall_Lag6_G57015 + 
             Rainfall_Lag1_G57004 + Rainfall_Lag2_G57004 + Rainfall_Lag3_G57004 + 
             Rainfall_Lag4_G57004 + Rainfall_Lag6_G57004 + Rainfall_Lag1_G57007 + 
             Rainfall_Lag2_G57007 + Rainfall_Lag3_G57007 + Rainfall_Lag4_G57007 + 
             Rainfall_Lag2_G57006 + Rainfall_Lag3_G57006 + Rainfall_Lag4_G57006 + 
             Rainfall_Lag1_G57005 + Rainfall_Lag2_G57005 + Rainfall_Lag3_G57005 + 
             Rainfall_Lag5_G57005, data = train_data[, response_predictor_columns])

lm2_predictor_columns <- c(
  response_column,
  "Flow_Lag2_G57002", "Flow_Lag3_G57002", "Flow_Lag4_G57002", "Flow_Lag6_G57002",
  "Flow_Lag1_G57015", "Flow_Lag3_G57015", "Flow_Lag4_G57015", "Flow_Lag6_G57015",
  "Flow_Lag3_G57004", "Flow_Lag6_G57004", "Flow_Lag1_G57007", "Flow_Lag2_G57007",
  "Flow_Lag3_G57007", "Flow_Lag5_G57007", "Flow_Lag1_G57006", "Flow_Lag3_G57006",
  "Flow_Lag4_G57006", "Flow_Lag5_G57006", "Flow_Lag6_G57006", "Flow_Lag1_G57005",
  "Flow_Lag2_G57005", "Flow_Lag3_G57005", "Flow_Lag4_G57005", "Flow_Lag5_G57005",
  "Flow_Lag6_G57005", "Rainfall_Lag1_G57002", "Rainfall_Lag4_G57002", "Rainfall_Lag5_G57002",
  "Rainfall_Lag6_G57002", "Rainfall_Lag1_G57015", "Rainfall_Lag2_G57015", "Rainfall_Lag4_G57015",
  "Rainfall_Lag6_G57015", "Rainfall_Lag1_G57004", "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004",
  "Rainfall_Lag4_G57004", "Rainfall_Lag6_G57004", "Rainfall_Lag1_G57007", "Rainfall_Lag2_G57007",
  "Rainfall_Lag3_G57007", "Rainfall_Lag4_G57007", "Rainfall_Lag2_G57006", "Rainfall_Lag3_G57006",
  "Rainfall_Lag4_G57006", "Rainfall_Lag1_G57005", "Rainfall_Lag2_G57005", "Rainfall_Lag3_G57005",
  "Rainfall_Lag5_G57005"
)



summary(lm2)



par(mfrow=c(2,3))
plot(lm2, which=1:6)

checkresiduals(lm2)


# Generate the forecast using the linear regression model on the test data
lm2_forecast_values <- predict(lm2, newdata = test_data)

# Calculate accuracy metrics for the forecast
lm2_mae <- mean(abs(lm2_forecast_values - test_data$Flow))
lm2_rmse <- sqrt(mean((lm2_forecast_values - test_data$Flow)^2))
lm2_mape <- mean(abs((lm2_forecast_values - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_mape, "%\n")



# Create a data frame containing the test_data and forecast_values
lm2_scatter_data <- data.frame(Date = test_data$Date, 
                               Observed = test_data$Flow,
                               Forecasted = lm2_forecast_values)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow") +
  theme_minimal()



################################################################################################################33

#  Build the linear model with seasonal intercepts :
lm3 <- lm(Flow ~ . + Month, data = train_data[, c(lm2_predictor_columns, "Month")])
summary(lm3)

par(mfrow=c(2,3))
plot(lm3, which=1:6)

checkresiduals(lm3)

# Generate the forecast using the linear regression model on the test data
lm3_forecast_values <- predict(lm3, newdata = test_data)

# Calculate accuracy metrics for the forecast
lm3_mae <- mean(abs(lm3_forecast_values - test_data$Flow))
lm3_rmse <- sqrt(mean((lm3_forecast_values - test_data$Flow)^2))
lm3_mape <- mean(abs((lm3_forecast_values - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_mape, "%\n")



# Create a data frame containing the test_data and forecast_values
lm3_scatter_data <- data.frame(Date = test_data$Date, 
                               Observed = test_data$Flow,
                               Forecasted = lm3_forecast_values)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow") +
  theme_minimal()


###############################################################################################################


# with interaction between the varaible 

lm4 <- lm(Flow ~ . + .:. , data = train_data[,lm2_predictor_columns])

summary(lm4)



# Plot diagnostics for the linear regression model
par(mfrow=c(2, 3))
plot(lm4, which=1:6)

checkresiduals(lm4)

# Generate the forecast using the linear regression model on the test data
lm4_forecast_values <- predict(lm4, newdata = test_data)

# Calculate accuracy metrics for the forecast
lm4_mae <- mean(abs(lm4_forecast_values - test_data$Flow))
lm4_rmse <- sqrt(mean((lm4_forecast_values - test_data$Flow)^2))
lm4_mape <- mean(abs((lm4_forecast_values - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_mape, "%\n")




# Create a data frame containing the test_data and forecast_values
lm4_scatter_data <- data.frame(Date = test_data$Date, 
                               Observed = test_data$Flow,
                               Forecasted = lm4_forecast_values)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow") +
  theme_minimal()



################################################################################################################33


#### non-linear transformation log(x+1) :
# Apply log transformation to response and predictor variables
G57005_log <- G57005
G57005_log[response_predictor_columns] <- log(G57005_log[response_predictor_columns] + 1)



# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
n <- nrow(G57005_log)
test_size <- round(0.2 * n)  # 20% of the total data
test_indices <- (n - test_size + 1):n  # Select indices from the end for the test data
train_indices <- setdiff(1:n, test_indices)  # Exclude test indices to get training indices

train_data_log <- G57005_log[train_indices, ]
test_data_log <- G57005_log[test_indices, ]


# Fit the linear regression model with log-transformed predictor variables
lm1_log <- lm(Flow ~ . , data = train_data_log[,response_predictor_columns])

summary(lm1_log)


par(mfrow=c(2,3))
plot(lm1_log, which=1:6)

checkresiduals(lm1_log)


# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_log <- predict(lm1_log, newdata = test_data_log)

# Apply the inverse of the log(x + 1) transformation to forecasted values
lm1_inv_log_forecast_values <- exp(lm1_forecast_values_log) - 1

# Create a data frame containing the test_data_log and forecast_values
lm1_log_scatter_data <- data.frame(Observed = test_data_log$Flow ,
                                   Forecasted = lm1_forecast_values_log)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_log_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Log-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_log_mae <- mean(abs(lm1_inv_log_forecast_values - exp(test_data_log$Flow) - 1))
lm1_log_rmse <- sqrt(mean((lm1_inv_log_forecast_values - exp(test_data_log$Flow) - 1)^2))
lm1_log_mape <- mean(abs((lm1_inv_log_forecast_values - exp(test_data_log$Flow) - 1) / exp(test_data_log$Flow) - 1)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_log_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_log_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_log_mape, "%\n")



# Using step function 
step(lm1_log)


lm2_log <- lm( Flow ~ Flow_Lag2_G57002 + Flow_Lag1_G57015 + Flow_Lag2_G57015 + 
                 Flow_Lag3_G57015 + Flow_Lag4_G57015 + Flow_Lag6_G57015 + 
                 Flow_Lag1_G57004 + Flow_Lag5_G57004 + Flow_Lag6_G57004 + 
                 Flow_Lag1_G57007 + Flow_Lag3_G57007 + Flow_Lag6_G57007 + 
                 Flow_Lag1_G57006 + Flow_Lag2_G57006 + Flow_Lag5_G57006 + 
                 Flow_Lag1_G57005 + Flow_Lag2_G57005 + Flow_Lag4_G57005 + 
                 Flow_Lag6_G57005 + Rainfall_Lag1_G57002 + Rainfall_Lag2_G57002 + 
                 Rainfall_Lag4_G57002 + Rainfall_Lag1_G57015 + Rainfall_Lag2_G57015 + 
                 Rainfall_Lag4_G57015 + Rainfall_Lag5_G57015 + Rainfall_Lag6_G57015 + 
                 Rainfall_Lag1_G57004 + Rainfall_Lag2_G57004 + Rainfall_Lag3_G57004 + 
                 Rainfall_Lag4_G57004 + Rainfall_Lag1_G57007 + Rainfall_Lag3_G57007 + 
                 Rainfall_Lag4_G57007 + Rainfall_Lag6_G57007 + Rainfall_Lag4_G57006 + 
                 Rainfall_Lag1_G57005 + Rainfall_Lag2_G57005 + Rainfall_Lag3_G57005 + 
                 Rainfall_Lag5_G57005, data = train_data_log[, response_predictor_columns])
summary(lm2_log)

log_predictor_columns <- c(
  response_column,
  "Flow_Lag2_G57002", "Flow_Lag1_G57015", "Flow_Lag2_G57015", "Flow_Lag3_G57015",
  "Flow_Lag4_G57015", "Flow_Lag6_G57015", "Flow_Lag1_G57004", "Flow_Lag5_G57004",
  "Flow_Lag6_G57004", "Flow_Lag1_G57007", "Flow_Lag3_G57007", "Flow_Lag6_G57007",
  "Flow_Lag1_G57006", "Flow_Lag2_G57006", "Flow_Lag5_G57006", "Flow_Lag1_G57005",
  "Flow_Lag2_G57005", "Flow_Lag4_G57005", "Flow_Lag6_G57005", "Rainfall_Lag1_G57002",
  "Rainfall_Lag2_G57002", "Rainfall_Lag4_G57002", "Rainfall_Lag1_G57015", "Rainfall_Lag2_G57015",
  "Rainfall_Lag4_G57015", "Rainfall_Lag5_G57015", "Rainfall_Lag6_G57015", "Rainfall_Lag1_G57004",
  "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004", "Rainfall_Lag4_G57004", "Rainfall_Lag1_G57007",
  "Rainfall_Lag3_G57007", "Rainfall_Lag4_G57007", "Rainfall_Lag6_G57007", "Rainfall_Lag4_G57006",
  "Rainfall_Lag1_G57005", "Rainfall_Lag2_G57005", "Rainfall_Lag3_G57005", "Rainfall_Lag5_G57005"
)


par(mfrow=c(2,3))
plot(lm2_log, which=1:6)

checkresiduals(lm2_log)

# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_log <- predict(lm2_log, newdata = test_data_log)

# Apply the inverse of the log(x + 1) transformation to forecasted values
lm2_inv_log_forecast_values <- exp(lm2_forecast_values_log) - 1

# Create a data frame containing the test_data_log and forecast_values
lm2_log_scatter_data <- data.frame(Observed = test_data_log$Flow ,
                                   Forecasted = lm2_forecast_values_log)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_log_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Log-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_log_mae <- mean(abs(lm2_inv_log_forecast_values - exp(test_data_log$Flow) - 1))
lm2_log_rmse <- sqrt(mean((lm2_inv_log_forecast_values - exp(test_data_log$Flow) - 1)^2))
lm2_log_mape <- mean(abs((lm2_inv_log_forecast_values - exp(test_data_log$Flow) - 1) / exp(test_data_log$Flow) - 1)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_log_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_log_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_log_mape, "%\n")




#  Build the linear model with seasonal intercepts for Gauge G57005



lm3_log <- lm(Flow ~ . + Month, data = train_data_log[, c(log_predictor_columns, "Month")])

summary(lm3_log)

par(mfrow=c(2,3))
plot(lm3_log, which=1:6)

checkresiduals(lm3_log)


# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_log <- predict(lm3_log, newdata = test_data_log)

# Apply the inverse of the log(x + 1) transformation to forecasted values
lm3_inv_log_forecast_values <- exp(lm3_forecast_values_log) - 1

# Create a data frame containing the test_data_log and forecast_values
lm3_log_scatter_data <- data.frame(Observed = test_data_log$Flow ,
                                   Forecasted = lm3_forecast_values_log)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_log_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Log-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_log_mae <- mean(abs(lm3_inv_log_forecast_values - exp(test_data_log$Flow) - 1))
lm3_log_rmse <- sqrt(mean((lm3_inv_log_forecast_values - exp(test_data_log$Flow) - 1)^2))
lm3_log_mape <- mean(abs((lm3_inv_log_forecast_values - exp(test_data_log$Flow) - 1) / exp(test_data_log$Flow) - 1)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_log_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_log_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_log_mape, "%\n")




# With interaction between the varaible 

lm4_log <- lm(Flow ~ . + .:. , data = train_data_log[,log_predictor_columns])

summary(lm4_log)


par(mfrow=c(2,3))
plot(lm4_log, which=1:6)

checkresiduals(lm4_log)

# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_log <- predict(lm4_log, newdata = test_data_log)

# Apply the inverse of the log(x + 1) transformation to forecasted values
lm4_inv_log_forecast_values <- exp(lm4_forecast_values_log) - 1

# Create a data frame containing the test_data_log and forecast_values
lm4_log_scatter_data <- data.frame(Observed = test_data_log$Flow ,
                                   Forecasted = lm4_forecast_values_log)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_log_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Log-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_log_mae <- mean(abs(lm4_inv_log_forecast_values - exp(test_data_log$Flow) - 1))
lm4_log_rmse <- sqrt(mean((lm4_inv_log_forecast_values - exp(test_data_log$Flow) - 1)^2))
lm4_log_mape <- mean(abs((lm4_inv_log_forecast_values - exp(test_data_log$Flow) - 1) / exp(test_data_log$Flow) - 1)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_log_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_log_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_log_mape, "%\n")



#####################################################################################################################################33




# Square roots transformation 

# Apply square root transformation to response and predictor variables
G57005_sqrt <- G57005

# Apply square root transformation to predictor columns
G57005_sqrt[, response_predictor_columns] <- sqrt(G57005_sqrt[, response_predictor_columns])

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)
n <- nrow(G57005_sqrt)
test_size <- round(0.2 * n)
test_indices <- (n - test_size + 1):n
train_indices <- setdiff(1:n, test_indices)

train_data_sqrt <- G57005_sqrt[train_indices, ]
test_data_sqrt <- G57005_sqrt[test_indices, ]


# Fit the linear regression model with square root-transformed predictor variables
lm1_sqrt <- lm(Flow ~ . , data = train_data_sqrt[,response_predictor_columns])

summary(lm1_sqrt)


par(mfrow=c(2,3))
plot(lm1_sqrt, which=1:6)

checkresiduals(lm1_sqrt)

# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_sqrt <- predict(lm1_sqrt, newdata = test_data_sqrt)

# Apply the inverse of the square root transformation to forecasted values
lm1_inv_sqrt_forecast_values <- (lm1_forecast_values_sqrt^2) 

# Create a data frame containing the test_data_log and forecast_values
lm1_sqrt_scatter_data <- data.frame(Observed = test_data_sqrt$Flow,
                                    Forecasted = lm1_forecast_values_sqrt)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_sqrt_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with sqrt- transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_sqrt_mae <- mean(abs(lm1_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)))
lm1_sqrt_rmse <- sqrt(mean((lm1_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2))^2))
lm1_sqrt_mape <- mean(abs((lm1_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)) / (test_data_sqrt$Flow^2))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_sqrt_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_sqrt_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_sqrt_mape, "%\n")




step(lm1_sqrt)


lm2_sqrt <- lm( Flow ~ Flow_Lag1_G57002 + Flow_Lag2_G57002 + Flow_Lag3_G57002 + 
                  Flow_Lag1_G57015 + Flow_Lag2_G57015 + Flow_Lag3_G57015 + 
                  Flow_Lag4_G57015 + Flow_Lag6_G57015 + Flow_Lag1_G57004 + 
                  Flow_Lag5_G57004 + Flow_Lag6_G57004 + Flow_Lag1_G57007 + 
                  Flow_Lag3_G57007 + Flow_Lag6_G57007 + Flow_Lag1_G57006 + 
                  Flow_Lag2_G57006 + Flow_Lag5_G57006 + Flow_Lag6_G57006 + 
                  Flow_Lag2_G57005 + Flow_Lag4_G57005 + Flow_Lag6_G57005 + 
                  Rainfall_Lag1_G57002 + Rainfall_Lag2_G57002 + Rainfall_Lag3_G57002 + 
                  Rainfall_Lag4_G57002 + Rainfall_Lag5_G57002 + Rainfall_Lag6_G57002 + 
                  Rainfall_Lag3_G57015 + Rainfall_Lag4_G57015 + Rainfall_Lag1_G57004 + 
                  Rainfall_Lag2_G57004 + Rainfall_Lag3_G57004 + Rainfall_Lag4_G57004 + 
                  Rainfall_Lag2_G57007 + Rainfall_Lag4_G57007 + Rainfall_Lag6_G57007 + 
                  Rainfall_Lag2_G57006 + Rainfall_Lag4_G57006 + Rainfall_Lag1_G57005 + 
                  Rainfall_Lag3_G57005 + Rainfall_Lag5_G57005, data = train_data_sqrt[, 
                                                                                      response_predictor_columns])

sqrt_predictor_columns <- c(
  response_column,
  "Flow_Lag1_G57002", "Flow_Lag2_G57002", "Flow_Lag3_G57002", "Flow_Lag1_G57015",
  "Flow_Lag2_G57015", "Flow_Lag3_G57015", "Flow_Lag4_G57015", "Flow_Lag6_G57015",
  "Flow_Lag1_G57004", "Flow_Lag5_G57004", "Flow_Lag6_G57004", "Flow_Lag1_G57007",
  "Flow_Lag3_G57007", "Flow_Lag6_G57007", "Flow_Lag1_G57006", "Flow_Lag2_G57006",
  "Flow_Lag5_G57006", "Flow_Lag6_G57006", "Flow_Lag2_G57005", "Flow_Lag4_G57005",
  "Flow_Lag6_G57005", "Rainfall_Lag1_G57002", "Rainfall_Lag2_G57002", "Rainfall_Lag3_G57002",
  "Rainfall_Lag4_G57002", "Rainfall_Lag5_G57002", "Rainfall_Lag6_G57002", "Rainfall_Lag3_G57015",
  "Rainfall_Lag4_G57015", "Rainfall_Lag1_G57004", "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004",
  "Rainfall_Lag4_G57004", "Rainfall_Lag2_G57007", "Rainfall_Lag4_G57007", "Rainfall_Lag6_G57007",
  "Rainfall_Lag2_G57006", "Rainfall_Lag4_G57006", "Rainfall_Lag1_G57005", "Rainfall_Lag3_G57005",
  "Rainfall_Lag5_G57005"
)

summary(lm2_sqrt)


par(mfrow=c(2,3))
plot(lm2_sqrt, which=1:6)

checkresiduals(lm1_sqrt)

# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_sqrt <- predict(lm2_sqrt, newdata = test_data_sqrt)

# Apply the inverse of the square root transformation to forecasted values
lm2_inv_sqrt_forecast_values <- (lm2_forecast_values_sqrt^2) 

# Create a data frame containing the test_data_log and forecast_values
lm2_sqrt_scatter_data <- data.frame(Observed = test_data_sqrt$Flow,
                                    Forecasted = lm2_forecast_values_sqrt)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_sqrt_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with sqrt- transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_sqrt_mae <- mean(abs(lm2_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)))
lm2_sqrt_rmse <- sqrt(mean((lm2_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2))^2))
lm2_sqrt_mape <- mean(abs((lm2_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)) / (test_data_sqrt$Flow^2))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_sqrt_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_sqrt_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_sqrt_mape, "%\n")


#  Build the linear model with seasonal intercepts for Gauge G57005

lm3_sqrt <- lm(Flow ~ . + Month, data = train_data_sqrt[, c(sqrt_predictor_columns, "Month")])


summary(lm3_sqrt)

par(mfrow=c(2,3))
plot(lm3_sqrt, which=1:6)

checkresiduals(lm3_sqrt)

# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_sqrt <- predict(lm3_sqrt, newdata = test_data_sqrt)

# Apply the inverse of the square root transformation to forecasted values
lm3_inv_sqrt_forecast_values <- (lm3_forecast_values_sqrt^2) 

# Create a data frame containing the test_data_log and forecast_values
lm3_sqrt_scatter_data <- data.frame(Observed = test_data_sqrt$Flow,
                                    Forecasted = lm3_forecast_values_sqrt)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_sqrt_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with sqrt- transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_sqrt_mae <- mean(abs(lm3_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)))
lm3_sqrt_rmse <- sqrt(mean((lm3_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2))^2))
lm3_sqrt_mape <- mean(abs((lm3_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)) / (test_data_sqrt$Flow^2))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_sqrt_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_sqrt_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_sqrt_mape, "%\n")

################################################################################################################
# With interaction between the varaible 

lm4_sqrt <- lm(Flow ~ . + .:. , data = train_data_sqrt[,sqrt_predictor_columns])


summary(lm4_sqrt)

par(mfrow=c(2,3))
plot(lm4_sqrt, which=1:6)

checkresiduals(lm4_sqrt)

# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_sqrt <- predict(lm4_sqrt, newdata = test_data_sqrt)

# Apply the inverse of the square root transformation to forecasted values
lm4_inv_sqrt_forecast_values <- (lm4_forecast_values_sqrt^2) 

# Create a data frame containing the test_data_log and forecast_values
lm4_sqrt_scatter_data <- data.frame(Observed = test_data_sqrt$Flow,
                                    Forecasted = lm4_forecast_values_sqrt)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_sqrt_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with sqrt- transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_sqrt_mae <- mean(abs(lm4_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)))
lm4_sqrt_rmse <- sqrt(mean((lm4_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2))^2))
lm4_sqrt_mape <- mean(abs((lm4_inv_sqrt_forecast_values - (test_data_sqrt$Flow^2)) / (test_data_sqrt$Flow^2))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_sqrt_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_sqrt_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_sqrt_mape, "%\n")


# Cubic root transformation

# Apply cubic root transformation to response and predictor variables
G57005_cubicroot <- G57005

# Apply cubic root transformation to predictor columns
G57005_cubicroot[, response_predictor_columns] <- sign(G57005_cubicroot[, response_predictor_columns]) * abs(G57005_cubicroot[, response_predictor_columns])^(1/3)

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)
n <- nrow(G57005_cubicroot)
test_size <- round(0.2 * n)
test_indices <- (n - test_size + 1):n
train_indices <- setdiff(1:n, test_indices)

train_data_cubicroot <- G57005_cubicroot[train_indices, ]
test_data_cubicroot <- G57005_cubicroot[test_indices, ]

# Fit the linear regression model with cubic root-transformed predictor variables
lm1_cubicr <- lm(Flow ~ . , data = train_data_cubicroot[, response_predictor_columns])

summary(lm1_cubicr)

par(mfrow=c(2,3))
plot(lm1_cubicr, which=1:6)


checkresiduals(lm1_cubicr)

# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_cubicr <- predict(lm1_cubicr, newdata = test_data_cubicroot)

# Apply the inverse of the cubic root transformation to forecasted values
lm1_inv_cubicr_forecast_values <- (lm1_forecast_values_cubicr^3)

# Create a data frame containing the test_data_cubicroot and forecast_values
lm1_cubicr_scatter_data <- data.frame(Observed = test_data_cubicroot$Flow,
                                      Forecasted = lm1_forecast_values_cubicr)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_cubicr_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Cubic Root-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_cubicr_mae <- mean(abs(lm1_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)))
lm1_cubicr_rmse <- sqrt(mean((lm1_inv_cubicr_forecast_values - (test_data_cubicroot$Flow^3))^2))
lm1_cubicr_mape <- mean(abs((lm1_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)) / (test_data_cubicroot$Flow^3))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_cubicr_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_cubicr_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_cubicr_mape, "%\n")


step(lm1_cubicr)


lm2_cubicr <- lm(Flow ~ Flow_Lag1_G57002 + Flow_Lag2_G57002 + Flow_Lag1_G57015 + 
                   Flow_Lag2_G57015 + Flow_Lag3_G57015 + Flow_Lag6_G57015 + 
                   Flow_Lag1_G57004 + Flow_Lag2_G57004 + Flow_Lag3_G57004 + 
                   Flow_Lag5_G57004 + Flow_Lag6_G57004 + Flow_Lag1_G57007 + 
                   Flow_Lag3_G57007 + Flow_Lag6_G57007 + Flow_Lag1_G57006 + 
                   Flow_Lag2_G57006 + Flow_Lag4_G57006 + Flow_Lag5_G57006 + 
                   Flow_Lag6_G57006 + Flow_Lag2_G57005 + Flow_Lag3_G57005 + 
                   Flow_Lag6_G57005 + Rainfall_Lag2_G57002 + Rainfall_Lag1_G57015 + 
                   Rainfall_Lag2_G57015 + Rainfall_Lag3_G57015 + Rainfall_Lag6_G57015 + 
                   Rainfall_Lag1_G57004 + Rainfall_Lag2_G57004 + Rainfall_Lag3_G57004 + 
                   Rainfall_Lag4_G57004 + Rainfall_Lag5_G57004 + Rainfall_Lag2_G57006 + 
                   Rainfall_Lag4_G57006 + Rainfall_Lag5_G57006 + Rainfall_Lag6_G57006 + 
                   Rainfall_Lag3_G57005 + Rainfall_Lag6_G57005, data = train_data_cubicroot[, 
                                                                                            response_predictor_columns])
cubicr_predictor_columns <- c(
  response_column,
  "Flow_Lag1_G57002", "Flow_Lag2_G57002", "Flow_Lag1_G57015", "Flow_Lag2_G57015",
  "Flow_Lag3_G57015", "Flow_Lag6_G57015", "Flow_Lag1_G57004", "Flow_Lag2_G57004",
  "Flow_Lag3_G57004", "Flow_Lag5_G57004", "Flow_Lag6_G57004", "Flow_Lag1_G57007",
  "Flow_Lag3_G57007", "Flow_Lag6_G57007", "Flow_Lag1_G57006", "Flow_Lag2_G57006",
  "Flow_Lag4_G57006", "Flow_Lag5_G57006", "Flow_Lag6_G57006", "Flow_Lag2_G57005",
  "Flow_Lag3_G57005", "Flow_Lag6_G57005", "Rainfall_Lag2_G57002", "Rainfall_Lag1_G57015",
  "Rainfall_Lag2_G57015", "Rainfall_Lag3_G57015", "Rainfall_Lag6_G57015", "Rainfall_Lag1_G57004",
  "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004", "Rainfall_Lag4_G57004", "Rainfall_Lag5_G57004",
  "Rainfall_Lag2_G57006", "Rainfall_Lag4_G57006", "Rainfall_Lag5_G57006", "Rainfall_Lag6_G57006",
  "Rainfall_Lag3_G57005", "Rainfall_Lag6_G57005"
)


summary(lm2_cubicr)

par(mfrow=c(2,3))
plot(lm2_cubicr, which=1:6)

checkresiduals(lm2_cubicr)

# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_cubicr <- predict(lm2_cubicr, newdata = test_data_cubicroot)

# Apply the inverse of the cubic root transformation to forecasted values
lm2_inv_cubicr_forecast_values <- (lm2_forecast_values_cubicr^3)

# Create a data frame containing the test_data_cubicroot and forecast_values
lm2_cubicr_scatter_data <- data.frame(Observed = test_data_cubicroot$Flow,
                                      Forecasted = lm2_forecast_values_cubicr)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_cubicr_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Cubic Root-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_cubicr_mae <- mean(abs(lm2_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)))
lm2_cubicr_rmse <- sqrt(mean((lm2_inv_cubicr_forecast_values - (test_data_cubicroot$Flow^3))^2))
lm2_cubicr_mape <- mean(abs((lm2_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)) / (test_data_cubicroot$Flow^3))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_cubicr_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_cubicr_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_cubicr_mape, "%\n")



#  Build the linear model with seasonal intercepts for Gauge G57005

lm3_cubicr <- lm(Flow ~ . + Month, data = train_data_cubicroot[, c(cubicr_predictor_columns, "Month")])

summary(lm3_cubicr)

par(mfrow=c(2,3))
plot(lm3_cubicr, which=1:6)

checkresiduals(lm3_cubicr)


# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_cubicr <- predict(lm3_cubicr, newdata = test_data_cubicroot)

# Apply the inverse of the cubic root transformation to forecasted values
lm3_inv_cubicr_forecast_values <- (lm3_forecast_values_cubicr^3)

# Create a data frame containing the test_data_cubicroot and forecast_values
lm3_cubicr_scatter_data <- data.frame(Observed = test_data_cubicroot$Flow,
                                      Forecasted = lm3_forecast_values_cubicr)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_cubicr_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Cubic Root-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_cubicr_mae <- mean(abs(lm3_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)))
lm3_cubicr_rmse <- sqrt(mean((lm3_inv_cubicr_forecast_values - (test_data_cubicroot$Flow^3))^2))
lm3_cubicr_mape <- mean(abs((lm3_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)) / (test_data_cubicroot$Flow^3))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_cubicr_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_cubicr_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_cubicr_mape, "%\n")




# With interaction between the varaible 

lm4_cubicr <- lm(Flow ~ . + .:. , data = train_data_cubicroot[,cubicr_predictor_columns])

summary(lm4_cubicr)


par(mfrow=c(2,3))
plot(lm4_cubicr, which=1:6)

checkresiduals(lm4_cubicr)

# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_cubicr <- predict(lm4_cubicr, newdata = test_data_cubicroot)

# Apply the inverse of the cubic root transformation to forecasted values
lm4_inv_cubicr_forecast_values <- (lm4_forecast_values_cubicr^3)

# Create a data frame containing the test_data_cubicroot and forecast_values
lm4_cubicr_scatter_data <- data.frame(Observed = test_data_cubicroot$Flow,
                                      Forecasted = lm4_forecast_values_cubicr)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_cubicr_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Cubic Root-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_cubicr_mae <- mean(abs(lm4_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)))
lm4_cubicr_rmse <- sqrt(mean((lm4_inv_cubicr_forecast_values - (test_data_cubicroot$Flow^3))^2))
lm4_cubicr_mape <- mean(abs((lm4_inv_cubicr_forecast_values  - (test_data_cubicroot$Flow^3)) / (test_data_cubicroot$Flow^3))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_cubicr_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_cubicr_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_cubicr_mape, "%\n")



###################################################################################################################################

# Apply square transformation to response and predictor variables
G57005_square <- G57005

# Apply square transformation to predictor columns
G57005_square[, response_predictor_columns] <- G57005_square[, response_predictor_columns]^2

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)
n <- nrow(G57005_square)
test_size <- round(0.2 * n)
test_indices <- (n - test_size + 1):n
train_indices <- setdiff(1:n, test_indices)

train_data_square <- G57005_square[train_indices, ]
test_data_square <- G57005_square[test_indices, ]

# Fit the linear regression model with square transformation to predictor variables
lm1_square <- lm(Flow ~ . , data = train_data_square[,response_predictor_columns])

summary(lm1_square)

par(mfrow=c(2,3))
plot(lm1_square, which=1:6)

checkresiduals(lm1_square)

# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_square <- predict(lm1_square, newdata = test_data_square)

# Apply the inverse of the square transformation to forecasted values
lm1_inv_square_forecast_values <- sqrt(abs(lm1_forecast_values_square))

# Create a data frame containing the test_data_square and forecast_values
lm1_square_scatter_data <- data.frame(Observed = test_data_square$Flow,
                                      Forecasted = lm1_forecast_values_square)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_square_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Square Transformation to Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_square_mae <- mean(abs(lm1_inv_square_forecast_values - sqrt(test_data_square$Flow)))
lm1_square_rmse <- sqrt(mean((lm1_inv_square_forecast_values - sqrt(test_data_square$Flow))^2))
lm1_square_mape <- mean(abs((lm1_inv_square_forecast_values - sqrt(test_data_square$Flow)) / sqrt(test_data_square$Flow))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_square_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_square_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_square_mape, "%\n")


step(lm1_square)


lm2_square <- lm(Flow ~ Flow_Lag1_G57002 + Flow_Lag2_G57002 + Flow_Lag4_G57002 + 
                   Flow_Lag5_G57002 + Flow_Lag6_G57002 + Flow_Lag1_G57015 + 
                   Flow_Lag2_G57015 + Flow_Lag3_G57015 + Flow_Lag4_G57015 + 
                   Flow_Lag5_G57015 + Flow_Lag1_G57004 + Flow_Lag2_G57004 + 
                   Flow_Lag3_G57004 + Flow_Lag4_G57004 + Flow_Lag5_G57004 + 
                   Flow_Lag1_G57007 + Flow_Lag3_G57007 + Flow_Lag6_G57007 + 
                   Flow_Lag1_G57006 + Flow_Lag2_G57006 + Flow_Lag3_G57006 + 
                   Flow_Lag4_G57006 + Flow_Lag6_G57006 + Flow_Lag1_G57005 + 
                   Flow_Lag2_G57005 + Flow_Lag3_G57005 + Flow_Lag4_G57005 + 
                   Flow_Lag6_G57005 + Rainfall_Lag1_G57002 + Rainfall_Lag2_G57002 + 
                   Rainfall_Lag3_G57002 + Rainfall_Lag4_G57002 + Rainfall_Lag5_G57002 + 
                   Rainfall_Lag1_G57015 + Rainfall_Lag2_G57015 + Rainfall_Lag3_G57015 + 
                   Rainfall_Lag4_G57015 + Rainfall_Lag1_G57004 + Rainfall_Lag2_G57004 + 
                   Rainfall_Lag3_G57004 + Rainfall_Lag4_G57004 + Rainfall_Lag6_G57004 + 
                   Rainfall_Lag1_G57007 + Rainfall_Lag3_G57007 + Rainfall_Lag6_G57007 + 
                   Rainfall_Lag1_G57006 + Rainfall_Lag3_G57006 + Rainfall_Lag4_G57006 + 
                   Rainfall_Lag5_G57006 + Rainfall_Lag6_G57006 + Rainfall_Lag1_G57005 + 
                   Rainfall_Lag2_G57005 + Rainfall_Lag3_G57005 + Rainfall_Lag4_G57005 + 
                   Rainfall_Lag5_G57005 + Rainfall_Lag6_G57005, data = train_data_square[, 
                                                                                         response_predictor_columns])

square_predictor_columns <- c(
  response_column,
  "Flow_Lag1_G57002", "Flow_Lag2_G57002", "Flow_Lag4_G57002", "Flow_Lag5_G57002",
  "Flow_Lag6_G57002", "Flow_Lag1_G57015", "Flow_Lag2_G57015", "Flow_Lag3_G57015",
  "Flow_Lag4_G57015", "Flow_Lag5_G57015", "Flow_Lag1_G57004", "Flow_Lag2_G57004",
  "Flow_Lag3_G57004", "Flow_Lag4_G57004", "Flow_Lag5_G57004", "Flow_Lag1_G57007",
  "Flow_Lag3_G57007", "Flow_Lag6_G57007", "Flow_Lag1_G57006", "Flow_Lag2_G57006",
  "Flow_Lag3_G57006", "Flow_Lag4_G57006", "Flow_Lag6_G57006", "Flow_Lag1_G57005",
  "Flow_Lag2_G57005", "Flow_Lag3_G57005", "Flow_Lag4_G57005", "Flow_Lag6_G57005",
  "Rainfall_Lag1_G57002", "Rainfall_Lag2_G57002", "Rainfall_Lag3_G57002", "Rainfall_Lag4_G57002",
  "Rainfall_Lag5_G57002", "Rainfall_Lag1_G57015", "Rainfall_Lag2_G57015", "Rainfall_Lag3_G57015",
  "Rainfall_Lag4_G57015", "Rainfall_Lag1_G57004", "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004",
  "Rainfall_Lag4_G57004", "Rainfall_Lag6_G57004", "Rainfall_Lag1_G57007", "Rainfall_Lag3_G57007",
  "Rainfall_Lag6_G57007", "Rainfall_Lag1_G57006", "Rainfall_Lag3_G57006", "Rainfall_Lag4_G57006",
  "Rainfall_Lag5_G57006", "Rainfall_Lag6_G57006", "Rainfall_Lag1_G57005", "Rainfall_Lag2_G57005",
  "Rainfall_Lag3_G57005", "Rainfall_Lag4_G57005", "Rainfall_Lag5_G57005", "Rainfall_Lag6_G57005"
)



summary(lm2_square)




par(mfrow=c(2,3))
plot(lm2_square, which=1:6)

checkresiduals(lm2_square)


# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_square <- predict(lm2_square, newdata = test_data_square)

# Apply the inverse of the square transformation to forecasted values
lm2_inv_square_forecast_values <- sqrt(abs(lm2_forecast_values_square))

# Create a data frame containing the test_data_square and forecast_values
lm2_square_scatter_data <- data.frame(Observed = test_data_square$Flow,
                                      Forecasted = lm2_forecast_values_square)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_square_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Square Transformation to Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_square_mae <- mean(abs(lm2_inv_square_forecast_values - sqrt(test_data_square$Flow)))
lm2_square_rmse <- sqrt(mean((lm2_inv_square_forecast_values - sqrt(test_data_square$Flow))^2))
lm2_square_mape <- mean(abs((lm2_inv_square_forecast_values - sqrt(test_data_square$Flow)) / sqrt(test_data_square$Flow))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_square_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_square_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_square_mape, "%\n")



#  Build the linear model with seasonal intercepts for Gauge G57005

lm3_square <- lm(Flow ~ . + Month, data = train_data_square[, c(square_predictor_columns, "Month")])

summary(lm3_square)


par(mfrow=c(2,3))
plot(lm3_square, which=1:6)

checkresiduals(lm3_square)


# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_square <- predict(lm3_square, newdata = test_data_square)

# Apply the inverse of the square transformation to forecasted values
lm3_inv_square_forecast_values <- sqrt(abs(lm3_forecast_values_square))

# Create a data frame containing the test_data_square and forecast_values
lm3_square_scatter_data <- data.frame(Observed = test_data_square$Flow,
                                      Forecasted = lm3_forecast_values_square)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_square_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Square Transformation to Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_square_mae <- mean(abs(lm3_inv_square_forecast_values - sqrt(test_data_square$Flow)))
lm3_square_rmse <- sqrt(mean((lm3_inv_square_forecast_values - sqrt(test_data_square$Flow))^2))
lm3_square_mape <- mean(abs((lm3_inv_square_forecast_values - sqrt(test_data_square$Flow)) / sqrt(test_data_square$Flow))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_square_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_square_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_square_mape, "%\n")

#######################



# With interaction between the varaible 

lm4_square <- lm(Flow ~ . + .:. , data = train_data_square[,square_predictor_columns])

summary(lm4_square)

par(mfrow=c(2,3))
plot(lm4_square, which=1:6)

checkresiduals(lm4_square)


# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_square <- predict(lm4_square, newdata = test_data_square)

# Apply the inverse of the square transformation to forecasted values
lm4_inv_square_forecast_values <- sqrt(abs(lm4_forecast_values_square))

# Create a data frame containing the test_data_square and forecast_values
lm4_square_scatter_data <- data.frame(Observed = test_data_square$Flow,
                                      Forecasted = lm4_forecast_values_square)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_square_scatter_data, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with Square Transformation to Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_square_mae <- mean(abs(lm4_inv_square_forecast_values - sqrt(test_data_square$Flow)))
lm4_square_rmse <- sqrt(mean((lm4_inv_square_forecast_values - sqrt(test_data_square$Flow))^2))
lm4_square_mape <- mean(abs((lm4_inv_square_forecast_values - sqrt(test_data_square$Flow)) / sqrt(test_data_square$Flow))) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_square_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_square_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_square_mape, "%\n")



#####################################################################################################################################33


G57005_inverse <- G57005

# Apply the inverse transformation to the log-transformed predictor columns
G57005_inverse[, response_predictor_columns] <- 1 / (G57005_inverse[, response_predictor_columns] + 1)

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
n <- nrow(G57005_inverse)
test_size <- round(0.2 * n)  # 20% of the total data
test_indices <- (n - test_size + 1):n  # Select indices from the end for the test data
train_indices <- setdiff(1:n, test_indices)  # Exclude test indices to get training indices

train_data_inverse <- G57005_inverse[train_indices, ]
test_data_inverse <- G57005_inverse[test_indices, ]

# Fit the linear regression model with log-transformed predictor variables
lm1_inverse <- lm(Flow ~ . , data = train_data_inverse[ ,response_predictor_columns])

summary(lm1_inverse)


par(mfrow=c(2,3))
plot(lm1_inverse, which=1:6)

checkresiduals(lm1_inverse)

# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_inverse <- predict(lm1_inverse, newdata = test_data_inverse)

# Apply the inverse of the 1/(x + 1) transformation to forecasted values
lm1_inv_inverse_forecast_values <- 1 / lm1_forecast_values_inverse - 1

# Create a data frame containing the test_data_log and forecast_values
lm1_inverse_scatter_data <- data.frame(Observed = test_data_inverse$Flow ,
                                       Forecasted = lm1_forecast_values_inverse)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_inverse_scatter_data , aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with inverse-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_inverse_mae <- mean(abs(lm1_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1)))
lm1_inverse_rmse <- sqrt(mean((lm1_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1))^2))
lm1_inverse_mape <- mean(abs((lm1_inv_inverse_forecast_values- (1 / test_data_inverse$Flow - 1)) / (1 / test_data_inverse$Flow - 1)) * 100)


# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm1_inverse_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm1_inverse_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm1_inverse_mape, "%\n")


step(lm1_inverse)

lm2_inverse <- lm(Flow ~ Flow_Lag1_G57002 + Flow_Lag2_G57002 + Flow_Lag3_G57015 + 
                    Flow_Lag1_G57004 + Flow_Lag3_G57004 + Flow_Lag4_G57004 + 
                    Flow_Lag5_G57004 + Flow_Lag6_G57004 + Flow_Lag1_G57007 + 
                    Flow_Lag4_G57007 + Flow_Lag6_G57007 + Flow_Lag1_G57006 + 
                    Flow_Lag2_G57006 + Flow_Lag4_G57006 + Flow_Lag5_G57006 + 
                    Flow_Lag1_G57005 + Flow_Lag2_G57005 + Flow_Lag3_G57005 + 
                    Rainfall_Lag1_G57002 + Rainfall_Lag2_G57002 + Rainfall_Lag1_G57015 + 
                    Rainfall_Lag2_G57015 + Rainfall_Lag1_G57004 + Rainfall_Lag3_G57004 + 
                    Rainfall_Lag1_G57007 + Rainfall_Lag2_G57007 + Rainfall_Lag3_G57007 + 
                    Rainfall_Lag2_G57006 + Rainfall_Lag1_G57005 + Rainfall_Lag3_G57005 + 
                    Rainfall_Lag4_G57005 + Rainfall_Lag5_G57005, data = train_data_inverse[, 
                                                                                           response_predictor_columns])


inverse_predictor_columns <- c(
  response_column,
  "Flow_Lag1_G57002", "Flow_Lag2_G57002", "Flow_Lag3_G57015",
  "Flow_Lag1_G57004", "Flow_Lag3_G57004", "Flow_Lag4_G57004", "Flow_Lag5_G57004", "Flow_Lag6_G57004",
  "Flow_Lag1_G57007", "Flow_Lag4_G57007", "Flow_Lag6_G57007", "Flow_Lag1_G57006",
  "Flow_Lag2_G57006", "Flow_Lag4_G57006", "Flow_Lag5_G57006", "Flow_Lag1_G57005",
  "Flow_Lag2_G57005", "Flow_Lag3_G57005", "Rainfall_Lag1_G57002", "Rainfall_Lag2_G57002",
  "Rainfall_Lag1_G57015", "Rainfall_Lag2_G57015", "Rainfall_Lag1_G57004", "Rainfall_Lag3_G57004",
  "Rainfall_Lag1_G57007", "Rainfall_Lag2_G57007", "Rainfall_Lag3_G57007", "Rainfall_Lag2_G57006",
  "Rainfall_Lag1_G57005", "Rainfall_Lag3_G57005", "Rainfall_Lag4_G57005", "Rainfall_Lag5_G57005"
)

summary(lm2_inverse)

par(mfrow=c(2,3))
plot(lm2_inverse, which=1:6)

checkresiduals(lm2_inverse)

# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_inverse <- predict(lm2_inverse, newdata = test_data_inverse)

# Apply the inverse of the 1/(x + 1) transformation to forecasted values
lm2_inv_inverse_forecast_values <- 1 / lm2_forecast_values_inverse - 1

# Create a data frame containing the test_data_log and forecast_values
lm2_inverse_scatter_data <- data.frame(Observed = test_data_inverse$Flow ,
                                       Forecasted = lm2_forecast_values_inverse)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_inverse_scatter_data , aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with inverse-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_inverse_mae <- mean(abs(lm2_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1)))
lm2_inverse_rmse <- sqrt(mean((lm2_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1))^2))
lm2_inverse_mape <- mean(abs((lm2_inv_inverse_forecast_values- (1 / test_data_inverse$Flow - 1)) / (1 / test_data_inverse$Flow - 1)) * 100)


# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm2_inverse_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm2_inverse_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm2_inverse_mape, "%\n")



#  Build the linear model with seasonal intercepts for Gauge G57005

lm3_inverse <- lm(Flow ~ . +  Month, data = train_data_inverse[, c(inverse_predictor_columns, "Month")])

summary(lm3_inverse)

par(mfrow=c(2,3))
plot(lm3_inverse, which=1:6)

checkresiduals(lm3_inverse)

# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_inverse <- predict(lm3_inverse, newdata = test_data_inverse)

# Apply the inverse of the 1/(x + 1) transformation to forecasted values
lm3_inv_inverse_forecast_values <- 1 / lm3_forecast_values_inverse - 1

# Create a data frame containing the test_data_log and forecast_values
lm3_inverse_scatter_data <- data.frame(Observed = test_data_inverse$Flow ,
                                       Forecasted = lm3_forecast_values_inverse)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_inverse_scatter_data , aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with inverse-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_inverse_mae <- mean(abs(lm3_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1)))
lm3_inverse_rmse <- sqrt(mean((lm3_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1))^2))
lm3_inverse_mape <- mean(abs((lm3_inv_inverse_forecast_values- (1 / test_data_inverse$Flow - 1)) / (1 / test_data_inverse$Flow - 1)) * 100)


# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm3_inverse_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm3_inverse_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm3_inverse_mape, "%\n")







# With interaction between the varaible 

lm4_inverse <- lm(Flow ~ . + .:. , data = train_data_inverse[,inverse_predictor_columns])

summary(lm4_inverse)

par(mfrow=c(2,3))
plot(lm4_inverse, which=1:6)


checkresiduals(lm4_inverse)

par(mfrow=c(1,1))

residuals__inverse <- residuals(lm4_inverse)
# Using base graphics
hist(residuals__inverse, main = "Residual Histogram", xlab = "Residuals")


# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_inverse <- predict(lm4_inverse, newdata = test_data_inverse)

# Apply the inverse of the 1/(x + 1) transformation to forecasted values
lm4_inv_inverse_forecast_values <- 1 / lm4_forecast_values_inverse - 1

# Create a data frame containing the test_data_log and forecast_values
lm4_inverse_scatter_data <- data.frame(Observed = test_data_inverse$Flow ,
                                       Forecasted = lm4_forecast_values_inverse)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_inverse_scatter_data , aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow", y = "Forecasted Flow", title = "Scatter Plot of Observed vs. Forecasted Flow with inverse-Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_inverse_mae <- mean(abs(lm4_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1)))
lm4_inverse_rmse <- sqrt(mean((lm4_inv_inverse_forecast_values - (1 / test_data_inverse$Flow - 1))^2))
lm4_inverse_mape <- mean(abs((lm4_inv_inverse_forecast_values- (1 / test_data_inverse$Flow - 1)) / (1 / test_data_inverse$Flow - 1)) * 100)


# Print accuracy metrics
cat("Mean Absolute Error (MAE):", lm4_inverse_mae, "\n")
cat("Root Mean Squared Error (RMSE):", lm4_inverse_rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", lm4_inverse_mape, "%\n")




#############################################################################



G57005_cox <- G57005

# Apply the Box-Cox transformation to the response variable
response_transformed <- boxcox(G57005_cox$Flow ~ 1)
best_lambda <- response_transformed$x[which.max(response_transformed$y)]
print(best_lambda)

G57005_cox[response_predictor_columns] <- ((G57005_cox[response_predictor_columns] + 1)^best_lambda - 1) / best_lambda

# Split data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
n <- nrow(G57005_cox)
test_size <- round(0.2 * n)  # 20% of the total data
test_indices <- (n - test_size + 1):n  # Select indices from the end for the test data
train_indices <- setdiff(1:n, test_indices)  # Exclude test indices to get training indices

train_data_cox <- G57005_cox[train_indices, ]
test_data_cox <- G57005_cox[test_indices, ]

# Fit the linear regression model with log-transformed predictor variables
lm1_cox <- lm(Flow ~ . , data = train_data_cox[, response_predictor_columns])

summary(lm1_cox)

par(mfrow=c(2,3))
plot(lm1_cox, which=1:6)

checkresiduals(lm1_cox)

# Generate the forecast using the linear regression model on the test data
lm1_forecast_values_cox <- predict(lm1_cox, newdata = test_data_cox)

# Apply the inverse of the Box-Cox transformation to forecasted values
lm1_inv_cox_forecast_values <- ((lm1_forecast_values_cox * best_lambda) + 1)^(1 / best_lambda)

# Create a data frame containing the test_data_boxcox and forecast_values
lm1_scatter_data_cox <- data.frame(Observed = test_data_cox$Flow,
                                   Forecasted = lm1_forecast_values_cox)

# Plot the scatter plot for test data and forecasted values
ggplot(lm1_scatter_data_cox, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow (Box-Cox Transformed)", y = "Forecasted Flow (Box-Cox Transformed)", title = "Scatter Plot of Observed vs. Forecasted Flow with Box-Cox Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm1_mae_cox <- mean(abs(lm1_inv_cox_forecast_values  - test_data$Flow))
lm1_rmse_cox <- sqrt(mean((lm1_inv_cox_forecast_values  - test_data$Flow)^2))
lm1_mape_cox <- mean(abs((lm1_inv_cox_forecast_values  - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE) with Box-Cox transformed data:", lm1_mae_cox, "\n")
cat("Root Mean Squared Error (RMSE) with Box-Cox transformed data:", lm1_rmse_cox, "\n")
cat("Mean Absolute Percentage Error (MAPE) with Box-Cox transformed data:", lm1_mape_cox, "%\n")

step(lm1_cox)

lm2_cox <- lm( Flow ~ Flow_Lag1_G57002 + Flow_Lag2_G57002 + Flow_Lag1_G57015 + 
                 Flow_Lag2_G57015 + Flow_Lag3_G57015 + Flow_Lag6_G57015 + 
                 Flow_Lag5_G57004 + Flow_Lag6_G57004 + Flow_Lag6_G57007 + 
                 Flow_Lag1_G57006 + Flow_Lag2_G57006 + Flow_Lag4_G57006 + 
                 Flow_Lag6_G57006 + Flow_Lag1_G57005 + Flow_Lag2_G57005 + 
                 Flow_Lag6_G57005 + Rainfall_Lag1_G57002 + Rainfall_Lag2_G57002 + 
                 Rainfall_Lag4_G57002 + Rainfall_Lag1_G57015 + Rainfall_Lag2_G57015 + 
                 Rainfall_Lag4_G57015 + Rainfall_Lag6_G57015 + Rainfall_Lag1_G57004 + 
                 Rainfall_Lag2_G57004 + Rainfall_Lag3_G57004 + Rainfall_Lag4_G57004 + 
                 Rainfall_Lag1_G57007 + Rainfall_Lag3_G57007 + Rainfall_Lag4_G57007 + 
                 Rainfall_Lag5_G57007 + Rainfall_Lag4_G57006 + Rainfall_Lag6_G57006 + 
                 Rainfall_Lag1_G57005 + Rainfall_Lag2_G57005 + Rainfall_Lag3_G57005 + 
                 Rainfall_Lag5_G57005 + Rainfall_Lag6_G57005, data = train_data_cox[, 
                                                                                    response_predictor_columns])

cox_predictor_columns <- c(
  response_column,
  "Flow_Lag1_G57002", "Flow_Lag2_G57002", "Flow_Lag1_G57015", "Flow_Lag2_G57015",
  "Flow_Lag3_G57015", "Flow_Lag6_G57015", "Flow_Lag5_G57004", "Flow_Lag6_G57004",
  "Flow_Lag6_G57007", "Flow_Lag1_G57006", "Flow_Lag2_G57006", "Flow_Lag4_G57006",
  "Flow_Lag6_G57006", "Flow_Lag1_G57005", "Flow_Lag2_G57005", "Flow_Lag6_G57005",
  "Rainfall_Lag1_G57002", "Rainfall_Lag2_G57002", "Rainfall_Lag4_G57002", "Rainfall_Lag1_G57015",
  "Rainfall_Lag2_G57015", "Rainfall_Lag4_G57015", "Rainfall_Lag6_G57015", "Rainfall_Lag1_G57004",
  "Rainfall_Lag2_G57004", "Rainfall_Lag3_G57004", "Rainfall_Lag4_G57004", "Rainfall_Lag1_G57007",
  "Rainfall_Lag3_G57007", "Rainfall_Lag4_G57007", "Rainfall_Lag5_G57007", "Rainfall_Lag4_G57006",
  "Rainfall_Lag6_G57006", "Rainfall_Lag1_G57005", "Rainfall_Lag2_G57005", "Rainfall_Lag3_G57005",
  "Rainfall_Lag5_G57005", "Rainfall_Lag6_G57005"
)

summary(lm2_cox)

par(mfrow=c(2,3))
plot(lm2_cox, which=1:6)

checkresiduals(lm2_cox)

# Generate the forecast using the linear regression model on the test data
lm2_forecast_values_cox <- predict(lm2_cox, newdata = test_data_cox)

# Apply the inverse of the Box-Cox transformation to forecasted values
lm2_inv_cox_forecast_values <- ((lm2_forecast_values_cox * best_lambda) + 1)^(1 / best_lambda)

# Create a data frame containing the test_data_boxcox and forecast_values
lm2_scatter_data_cox <- data.frame(Observed = test_data_cox$Flow,
                                   Forecasted = lm2_forecast_values_cox)

# Plot the scatter plot for test data and forecasted values
ggplot(lm2_scatter_data_cox, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow (Box-Cox Transformed)", y = "Forecasted Flow (Box-Cox Transformed)", title = "Scatter Plot of Observed vs. Forecasted Flow with Box-Cox Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm2_mae_cox <- mean(abs(lm2_inv_cox_forecast_values  - test_data$Flow))
lm2_rmse_cox <- sqrt(mean((lm2_inv_cox_forecast_values  - test_data$Flow)^2))
lm2_mape_cox <- mean(abs((lm2_inv_cox_forecast_values  - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE) with Box-Cox transformed data:", lm2_mae_cox, "\n")
cat("Root Mean Squared Error (RMSE) with Box-Cox transformed data:", lm2_rmse_cox, "\n")
cat("Mean Absolute Percentage Error (MAPE) with Box-Cox transformed data:", lm2_mape_cox, "%\n")




#  Build the linear model with seasonal intercepts for Gauge G57005

lm3_cox <- lm(Flow ~ . + Month, data = train_data_cox[, c(cox_predictor_columns, "Month")])


summary(lm3_cox)

par(mfrow=c(2,3))
plot(lm3_cox, which=1:6)

checkresiduals(lm3_cox)

# Generate the forecast using the linear regression model on the test data
lm3_forecast_values_cox <- predict(lm3_cox, newdata = test_data_cox)

# Apply the inverse of the Box-Cox transformation to forecasted values
lm3_inv_cox_forecast_values <- ((lm3_forecast_values_cox * best_lambda) + 1)^(1 / best_lambda)

# Create a data frame containing the test_data_boxcox and forecast_values
lm3_scatter_data_cox <- data.frame(Observed = test_data_cox$Flow,
                                   Forecasted = lm3_forecast_values_cox)

# Plot the scatter plot for test data and forecasted values
ggplot(lm3_scatter_data_cox, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow (Box-Cox Transformed)", y = "Forecasted Flow (Box-Cox Transformed)", title = "Scatter Plot of Observed vs. Forecasted Flow with Box-Cox Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm3_mae_cox <- mean(abs(lm3_inv_cox_forecast_values  - test_data$Flow))
lm3_rmse_cox <- sqrt(mean((lm3_inv_cox_forecast_values  - test_data$Flow)^2))
lm3_mape_cox <- mean(abs((lm3_inv_cox_forecast_values  - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE) with Box-Cox transformed data:", lm3_mae_cox, "\n")
cat("Root Mean Squared Error (RMSE) with Box-Cox transformed data:", lm3_rmse_cox, "\n")
cat("Mean Absolute Percentage Error (MAPE) with Box-Cox transformed data:", lm3_mape_cox, "%\n")


# with interaction between the varaible 

lm4_cox <- lm(Flow ~ . + .:. , data = train_data_cox[ ,cox_predictor_columns ])

summary(lm4_cox)

par(mfrow=c(2,3))
plot(lm4_cox, which=1:6)

checkresiduals(lm4_cox)


# Generate the forecast using the linear regression model on the test data
lm4_forecast_values_cox <- predict(lm4_cox, newdata = test_data_cox)

# Apply the inverse of the Box-Cox transformation to forecasted values
lm4_inv_cox_forecast_values <- ((lm4_forecast_values_cox * best_lambda) + 1)^(1 / best_lambda)

# Create a data frame containing the test_data_boxcox and forecast_values
lm4_scatter_data_cox <- data.frame(Observed = test_data_cox$Flow,
                                   Forecasted = lm4_forecast_values_cox)

# Plot the scatter plot for test data and forecasted values
ggplot(lm4_scatter_data_cox, aes(x = Observed, y = Forecasted)) +
  geom_point(size = 3, col = "blue") +
  labs(x = "Observed Flow (Box-Cox Transformed)", y = "Forecasted Flow (Box-Cox Transformed)", title = "Scatter Plot of Observed vs. Forecasted Flow with Box-Cox Transformed Predictors") +
  theme_minimal()

# Calculate accuracy metrics for the forecast
lm4_mae_cox <- mean(abs(lm4_inv_cox_forecast_values  - test_data$Flow))
lm4_rmse_cox <- sqrt(mean((lm4_inv_cox_forecast_values  - test_data$Flow)^2))
lm4_mape_cox <- mean(abs((lm4_inv_cox_forecast_values  - test_data$Flow) / test_data$Flow)) * 100

# Print accuracy metrics
cat("Mean Absolute Error (MAE) with Box-Cox transformed data:", lm4_mae_cox, "\n")
cat("Root Mean Squared Error (RMSE) with Box-Cox transformed data:", lm4_rmse_cox, "\n")
cat("Mean Absolute Percentage Error (MAPE) with Box-Cox transformed data:", lm4_mape_cox, "%\n")