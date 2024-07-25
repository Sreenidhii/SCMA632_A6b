# Set working directory and load necessary libraries
setwd('D:\\CHRIST\\Boot camp\\DATA')
getwd()

# Load necessary libraries
library(readxl)
library(dplyr)
library(janitor)
library(urca)
library(vars)

# Load the dataset
df <- read_excel('pinksheet.xlsx', sheet = "Monthly Prices", skip = 6)

# Rename the first column to "Date"
colnames(df)[1] <- 'Date'
# Convert the Date column to Date format
df$Date <- as.Date(paste0(df$Date, "01"), format = "%YM%m%d")
str(df)

# Select specific columns (Date and selected commodities)
commodity <- df[,c(1,3,25,70,72,61,31)] %>%
  clean_names()

str(commodity)

# Remove the Date column for analysis
commodity_data <- dplyr::select(commodity, -date)

# Column names to test (if you want to specify particular columns)
columns_to_test <- names(commodity_data)

# Initialize counters and lists for stationary and non-stationary columns
non_stationary_count <- 0
stationary_columns <- list()
non_stationary_columns <- list()

# Loop through each column and perform the ADF test
for (col in columns_to_test) {
  adf_result <- ur.df(commodity_data[[col]], type = "none", selectlags = "AIC")
  p_value <- adf_result@testreg$coefficients[2, 4]  # Extract p-value for the test
  cat("\nADF test result for column:", col, "\n")
  print(summary(adf_result))
  
  # Check if the p-value is greater than 0.05 (commonly used threshold)
  if (p_value > 0.05) {
    non_stationary_count <- non_stationary_count + 1
    non_stationary_columns <- c(non_stationary_columns, col)
  } else {
    stationary_columns <- c(stationary_columns, col)
  }
}

# Print the number of non-stationary columns and the lists of stationary and non-stationary columns
cat("\nNumber of non-stationary columns:", non_stationary_count, "\n")
cat("Non-stationary columns:", non_stationary_columns, "\n")
cat("Stationary columns:")
stationary_columns

# Co-Integration Test (Johansen's Test)
# Determining the number of lags to use (you can use information criteria like AIC, BIC)
lags <- VARselect(commodity_data, lag.max = 10, type = "const")
lag_length <- lags$selection[1] # Choosing the lag with the lowest AIC

vecm_model <- ca.jo(commodity_data, ecdet = 'const', type = 'eigen', K = lag_length, spec = 'transitory')

# Summary of the Co-Integration Test
summary(vecm_model)

# Determine the number of co-integrating relationships (r) based on the test
# Here, we assume r = 1 if there's at least one significant eigenvalue
r <- 3 # Replace with the actual number from the test results

if (r > 0) {
  # If co-integration exists, estimate the VECM model
  vecm <- cajorls(vecm_model, r = r)  # r is the number of co-integration vectors
  
  # Summary of the VECM model
  summary(vecm)
  
  # Extracting the coefficients from the VECM model
  vecm_coefs <- vecm$rlm$coefficients
  print(vecm_coefs)
  
  # Creating a VAR model for prediction using the VECM
  vecm_pred <- vec2var(vecm_model, r = r)
  
  # Forecasting using the VECM model
  # Forecasting 12 steps ahead
  forecast <- predict(vecm_pred, n.ahead = 24)
  
  # Plotting the forecast
  par(mar = c(4, 4, 2, 2))  # Adjust margins: c(bottom, left, top, right)
  plot(forecast)
  
} else {
  # If no co-integration exists, proceed with Unrestricted VAR Analysis
  var_model <- VAR(commodity_data, p = lag_length, type = "const")
  
  # Summary of the VAR model
  summary(var_model)
  
  # Granger causality test
  causality_results <- causality(var_model)
  print(causality_results)
  
  # Forecasting using the VAR model
  forecast <- predict(var_model, n.ahead = 24)
  
  # Plotting the forecast
  par(mar = c(4, 4, 2, 2))  # Adjust margins: c(bottom, left, top, right)
  plot(forecast)
}

forecast

