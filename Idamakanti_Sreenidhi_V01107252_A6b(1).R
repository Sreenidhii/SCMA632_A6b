install.packages("rugarch")

# Load necessary libraries
library(quantmod)
library(rugarch)

# Download historical stock data
ticker <- "HDFCBANK.NS"
start_date <- as.Date("2021-04-01")
end_date <- as.Date("2024-03-31")
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)

# Calculate daily returns
data <- Cl(HDFCBANK.NS)  # Get adjusted close prices
returns <- diff(log(data)) * 100  # Calculate daily returns in percentage
returns <- na.omit(returns)  # Remove NA values

# Display the first few rows to verify the addition of returns
head(returns)

# Fit an ARCH model
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                        distribution.model = "norm")
arch_fit <- ugarchfit(spec = arch_spec, data = returns)
cat("\nARCH Model Summary:\n")
print(arch_fit)

# Plot the conditional volatility for ARCH
plot(arch_fit, which = "all", main = "Conditional Volatility of HDFC Bank Returns (ARCH)")

# Fit a GARCH model
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
garch_fit <- ugarchfit(spec = garch_spec, data = returns)
cat("\nGARCH Model Summary:\n")
print(garch_fit)

# Plot the conditional volatility for GARCH
plot(garch_fit, which = "all", main = "Conditional Volatility of HDFC Bank Returns (GARCH)")


# Forecast the three-month volatility (90 days)
forecast <- ugarchforecast(garch_fit, n.ahead = 90)

# Print forecasted mean
cat("\nForecasted Mean:\n")
print(head(fitted(forecast)))

# Assign the forecasted mean
forecasted_mean <- fitted(forecast)

# Plot the forecasted mean
plot(forecasted_mean, type = "l", col = "Blue", 
     main = "Forecasted Mean", xlab = "Time", ylab = "Mean")


#Print forecasted Residual Variance
cat("\nForecasted Residual Variance:\n")
print(head(sigma(forecast)^2))

# Assign the forecasted residual variance
forecasted_residual_variance <- sigma(forecast)^2

# Plot the forecasted residual variance
plot(forecasted_residual_variance, type = "l", col = "Red", 
     main = "Forecasted Residual Variance", xlab = "Time", ylab = "Residual Variance")


#Print Variance
cat("\nForecasted Variance:\n")
print(head(sigma(forecast)^2))

# Assign the forecasted variance
forecasted_variance <- sigma(forecast)^2

# Plot the forecasted variance
plot(forecasted_variance, type = "l", col = "Purple", 
     main = "Forecasted Variance", xlab = "Time", ylab = "Variance")

