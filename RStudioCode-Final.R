###First Dataset
#Question 1.1a: Read data and subset for a specific age

# Load required packages
library(gamlss)
library(MASS) 

# Load the BMI data
data(dbbmi)

# Define the selected age
selected_age <- 14

# Subset the data for the specified age
selected_data <- with(dbbmi, subset(dbbmi, age > selected_age & age < selected_age + 1))

# Extract BMI data for the specified age
bmi14 <- selected_data$bmi

par(mfrow = c(2, 2))
# Plot the histogram with different nbins values
truehist(bmi14, nbins = 10, main = "BMI Distribution (10 bins)")
truehist(bmi14, nbins = 15, main = "BMI Distribution (15 bins)")
truehist(bmi14, nbins = 20, main = "BMI Distribution (20 bins)")
truehist(bmi14, nbins = 30, main = "BMI Distribution (30 bins)")

par(mfrow = c(1, 1))

#Question 1.1b: Fit different parametric distributions and choose the best one using AIC
# Load required packages
library(fitdistrplus)
library(gamlss)      
library(MASS)        
library(ggplot2)       

# Fit parametric distributions
fit_normal <- fitdist(bmi14, "norm")
fit_lognormal <- fitdist(bmi14, "lnorm")
fit_weibull <- fitdist(bmi14, "weibull")
fit_gamma <- fitdist(bmi14, "gamma")

##Choose Best Distribution - Option No 1 (AIC Value)

# Calculate AIC for each distribution and print the values
cat("AIC values:\n")
cat("Normal Distribution:", fit_normal$aic, "\n")
cat("Log-normal Distribution:", fit_lognormal$aic, "\n")
cat("Weibull Distribution:", fit_weibull$aic, "\n")
cat("Gamma Distribution:", fit_gamma$aic, "\n")

# Identify the distribution with the lowest AIC
best_distribution <- c("Normal", "Log-normal", "Weibull", "Gamma")[which.min(c(fit_normal$aic, fit_lognormal$aic, fit_weibull$aic, fit_gamma$aic))]

# Print the best distribution based on AIC
cat("Best distribution based on AIC:", best_distribution, "with AIC value:", switch(best_distribution, "Normal" = fit_normal$aic, "Log-normal" = fit_lognormal$aic, "Weibull" = fit_weibull$aic, "Gamma" = fit_gamma$aic), "\n")

##Choose Best Distribution - Option No 2 (Density Curves)

# Plot histogram of BMI data
hist(bmi14, freq = FALSE, main = "Histogram of BMI Data with Fitted Distributions", xlab = "BMI", ylab = "Density")

# Add density curves for all distributions
x <- seq(min(bmi14), max(bmi14), length.out = 100)
curve(dnorm(x, mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]), col = "blue", lwd = 2, add = TRUE)
curve(dlnorm(x, meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), col = "red", lwd = 2, add = TRUE)
curve(dweibull(x, shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), col = "green", lwd = 2, add = TRUE)
curve(dgamma(x, shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), col = "orange", lwd = 2, add = TRUE)

# Add legend for all distributions
legend("topright", legend = c("Normal", "Log-normal", "Weibull", "Gamma"), col = c("blue", "red", "green", "orange"), lwd = 2)

# Add density curve for the best distribution
hist(bmi14, freq = FALSE, main = "Histogram of BMI Data with Fitted Distributions", xlab = "BMI", ylab = "Density")

if (best_distribution == "Normal") {
  curve(dnorm(x, mean = fit_normal$estimate[1], sd = fit_normal$estimate[2]), col = "blue", lwd = 2, add = TRUE)
  legend_color <- "blue"
} else if (best_distribution == "Log-normal") {
  curve(dlnorm(x, meanlog = fit_lognormal$estimate[1], sdlog = fit_lognormal$estimate[2]), col = "red", lwd = 2, add = TRUE)
  legend_color <- "red"
} else if (best_distribution == "Weibull") {
  curve(dweibull(x, shape = fit_weibull$estimate[1], scale = fit_weibull$estimate[2]), col = "green", lwd = 2, add = TRUE)
  legend_color <- "green"
} else if (best_distribution == "Gamma") {
  curve(dgamma(x, shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), col = "orange", lwd = 2, add = TRUE)
  legend_color <- "orange"
}

# Add legend for the best distribution 
legend("topright", legend = best_distribution, col = legend_color, lwd = 2)

##Choose Best Distribution - Option No 2 (Q-Q Plots)

# Q-Q plots for each distribution
par(mfrow = c(2, 2))

# Q-Q plot for Normal distribution
qqnorm(bmi14, main = "Q-Q Plot: Normal Distribution")
qqline(bmi14, col = "red")

# Q-Q plot for Log-normal distribution
qqnorm(log(bmi14), main = "Q-Q Plot: Log-normal Distribution")
qqline(log(bmi14), col = "red")

# Q-Q plot for Weibull distribution
qqnorm(bmi14, main = "Q-Q Plot: Weibull Distribution", dist = "weibull")
qqline(bmi14, col = "red")

# Q-Q plot for Gamma distribution
qqnorm(bmi14, main = "Q-Q Plot: Gamma Distribution", dist = "gamma")
qqline(bmi14, col = "red")

par(mfrow = c(1, 1))


#Question 1.1c: Output parameter estimates for the chosen model
summary(fit_lognormal)
#####################################################################################################

###Second Dataset
#Question 1.2a: Read the data file
# Load the required package
library(gamlss)

# Read the data file
data(grip)


#Question 1.2b: Select individual sample
# Set your own seed number
set.seed(987)

# Generate random indices for sampling
index <- sample(3766, 1000)

# Subset the grip data with the selected indices
mydata <- grip[index, ]

# Check the dimensions of the subset data
dim(mydata)


#Question 1.2c: Plot grip against age
# Plot grip against age
plot(mydata$age, mydata$grip, 
     xlab = "Age", ylab = "Grip Strength",
     main = "Grip Strength vs. Age")


#Question 1.2d: Fit the BCCG distribution for grip using the LMS method
# Fit the BCCG distribution for grip using the LMS method
gbccg <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), data = mydata, family = BCCG)

# Calculate the effective degrees of freedom
edf(gbccg)


#Question 1.2e: Fit the BCT and BCPE distributions using the fitted values from the LMS model as starting values
# Fit the BCT and BCPE distributions using the fitted values from the LMS model as starting values
gbct <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), 
               data = mydata, family = BCT, start.from = gbccg)

gbcp <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), tau.fo = ~pb(age), 
               data = mydata, family = BCPE, start.from = gbccg)

# Calculate the effective degrees of freedom for the parameters in the BCT and BCPE distributions
edf(gbct)
edf(gbcp)


#Question 1.2f: Compare models using GAIC
gaic_values <- c(GAIC(gbccg), GAIC(gbct), GAIC(gbcp))
model_names <- c("BCCG", "BCT", "BCPE")

# Print GAIC values for each model
cat("GAIC values:\n")
cat(paste(model_names, ":", gaic_values, "\n"))

# Find the index of the minimum GAIC value
best_model_index <- which.min(gaic_values)

# Print the best model with its GAIC value
cat("Best Model (lowest GAIC value):\n")
cat(paste(model_names[best_model_index], ":", gaic_values[best_model_index], "\n"))


#Question 1.2g: Plot fitted parameters for the fitted models
# Plot fitted parameters for the fitted models
library(gamlss)
fittedPlot(gbccg, gbct, x = mydata$age)

#Question No. 1.2h: Obtain centile plot for the fitted models
# Obtain centile plot for the fitted models

centiles_bccg <- centiles(gbccg)
centiles_bct <- centiles(gbct)
centiles_bcpe <- centiles(gbcp)

centiles_split <- list()
for (model in list(gbccg, gbct, gbcp)) {
  centiles_split[[length(centiles_split) + 1]] <- centiles.split(model, para = "sigma")
}

#Question 1.2i: Investigate residuals from the fitted models
# Investigate residuals from the fitted models
plot(gbccg, which = 2); title(main = "BCCG Model", cex.main = 1.2)
plot(gbct, which = 2); title(main = "BCT Model", cex.main = 1.2)
plot(gbcp, which = 2); title(main = "BCPE Model", cex.main = 1.2)

wp(gbccg); title(main = "Worm Plot (BCCG Model)", cex.main = 1.2)
wp(gbct); title(main = "Worm Plot (BCT Model)", cex.main = 1.2)
wp(gbcp); title(main = "Worm Plot (BCPE Model)", cex.main = 1.2)

Q.stats(gbccg); title(main = "Q-Statistics (BCCG Model)", cex.main = 1.2)
Q.stats(gbct); title(main = "Q-Statistics (BCT Model)", cex.main = 1.2)
Q.stats(gbcp); title(main = "Q-Statistics (BCPE Model)", cex.main = 1.2)

##Final Centile
par(mfrow = c(1, 1))
centiles_bct <- centiles(gbct)
#########################################################################################################
#Question No. 1.3b: Provide Dataset Information
# Load required libraries

library(quantmod)
library(openxlsx)

# Define the stock symbol and time period
stock_symbol <- "TSLA"
start_date <- as.Date("2020-01-01")
end_date <- Sys.Date()

# Download stock data from Yahoo Finance
getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date)

# Export stock data to Excel
write.xlsx(as.data.frame(TSLA), file = "Tesla_Stock_Data.xlsx")

#View data
TSLA
colnames(TSLA)
str(TSLA)

# Check for missing values in all data
any(is.na(TSLA))
# Handle missing values in all data
TSLA <- na.omit(TSLA)

#Question No. 1.3c: Preliminary Data Analysis
# Plot closing prices
chartSeries(Cl(TSLA), type = "line", theme = "white", name = "Tesla Stock Closing Prices")
# Plot closing prices using base R plot function
plot(index(TSLA), coredata(Cl(TSLA)), type = "l", col = "blue",
     main = "Tesla Stock Closing Prices", xlab = "Date", ylab = "Closing Price")

# Plot trading volume
chartSeries(Vo(TSLA), type = "line", theme = "white", name = "Tesla Stock Trading Volume")
# Plot Tesla stock trading volume
plot(index(TSLA), coredata(Vo(TSLA)), type = "l", col = "blue",
     main = "Tesla Stock Trading Volume", xlab = "Date", ylab = "Trading Volume")

# Additional Statistical Analysis
# Calculate summary statistics for Tesla's closing prices
closing_prices <- Cl(TSLA)
summary_stats <- summary(closing_prices)
print(summary_stats)

library(e1071)
# Calculate skewness of Tesla's closing prices
closing_skewness <- skewness(closing_prices)
print(paste("Skewness of closing prices:", closing_skewness))

# Correlation Analysis
# Calculate correlation between Tesla's closing prices and trading volume
closing_volume_correlation <- cor(Cl(TSLA), Vo(TSLA))
print(paste("Correlation between closing prices and trading volume:", closing_volume_correlation))

# Print summary of the dataset
summary(TSLA)

#Question No. 1.3d: Find an appropriate statistical model
# 1. Load necessary libraries
library(quantmod)
#library(caret)

# 2. Define Variables
response_var <- Cl(TSLA)
explanatory_vars <- data.frame(Volume = Vo(TSLA), Open = Op(TSLA), High = Hi(TSLA), Low = Lo(TSLA))

# 3. Fit Statistical Model (Linear Regression)
model <- lm(response_var ~ ., data = explanatory_vars)

# 4. Assess Model Fit and Check Assumptions
summary(model)


par(mfrow = c(2, 2))
plot(model)

# 5. Interpret Results
coefficients(model)

# 6. Train Model and Perform Cross-Validation
# Train model
model <- lm(TSLA.Close ~ ., data = train_data)
# Perform cross-validation
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
model_cv <- train(TSLA.Close ~ ., data = train_data, method = "lm", trControl = train_control)
print(model_cv)

#Question No. 1.3e: Check assumptions of the linear regression model
# Arrange plots in a 2x2 grid
par(mfrow = c(2, 2))  
plot(model, which = 1)  # Residuals vs. Fitted Values Plot
plot(model, which = 2)  # Normal Q-Q Plot
plot(model, which = 3)  # Scale-Location Plot
plot(model, which = 5)  # Residuals vs. Leverage Plot

par(mfrow = c(1, 1)) 

#Question No. 1.3f: Use model for predictions
# Step 1: Set seed for reproducibility
set.seed(123)

# Step 2: Split the data into training and testing sets (80% training, 20% testing)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data), replace = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Step 3: Fit the model using the training data
model <- lm(TSLA.Close ~ ., data = train_data)

# Step 4: Make predictions on the testing data
predictions <- predict(model, newdata = test_data)

# Extract the response variable from the testing data
actual_values <- test_data$TSLA.Close

# Step 5: Calculate and print RMSE
cat("RMSE:", sqrt(mean((actual_values - predictions)^2)), "\n")

# Step 6: Calculate and print R-squared
cat("R-squared:", summary(model)$r.squared, "\n")

# Step 7: Create a data frame with actual and predicted values
results <- data.frame(Actual = actual_values, Predicted = predictions)

# Step 8: Print the first few rows of the results
head(results)

# Step 9: Plot actual and predicted prices over time
library(ggplot2)

results <- data.frame(Date = index(test_data), Actual = test_data$TSLA.Close, Predicted = predictions)

ggplot(results, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Actual vs. Predicted Closing Prices of TSLA",
       y = "Price",
       color = "Price Type") +
  theme_minimal()

library(ggplot2)

# Plot
ggplot(results, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +  # Make the actual line bold
  geom_line(aes(y = Predicted, color = "Predicted", linetype = "Predicted"), linetype = "dashed", size = 1) +  # Make the predicted line dashed and add it to the legend
  scale_color_manual(name = "Price Type", values = c("Actual" = "red", "Predicted" = "sky blue")) +  # Customize color legend
  scale_linetype_manual(name = "Price Type", values = c("Actual" = "solid", "Predicted" = "dashed")) +  # Customize linetype legend
  labs(title = "Actual vs. Predicted Closing Prices of TSLA",
       y = "Price",
       color = "Price Type",
       linetype = "Price Type") +  # Add linetype to the legend
  theme_minimal()


########################################################################################################################