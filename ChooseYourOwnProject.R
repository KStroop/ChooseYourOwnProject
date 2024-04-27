##########################################################
# Data preparation
##########################################################

# Install lightgbm and glmnet packages
if(!require(lightgbm)) install.packages("lightgbm", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")

# Load all required libraries
library(lightgbm)
library(glmnet)
library(httr)
library(ggplot2)
library(dplyr)

# Define the URL
url <- "https://github.com/KStroop/ChooseYourOwnProject/raw/main/corona-virus-report.zip"

# Use httr to handle the download
GET(url, write_disk("corona-virus-report.zip", overwrite = TRUE))

# Proceed to check the file and unzip as before
if (!file.exists("corona-virus-report.zip") || file.size("corona-virus-report.zip") == 0) {
  stop("Download failed: file is missing or empty")
}

unzip("corona-virus-report.zip", exdir = "data")

# Check if the unzipping was successful and the file exists
if (!file.exists("data/usa_county_wise.csv")) {
  stop("Unzipping failed: data/usa_county_wise.csv does not exist")
}

# Read the csv into covid_data
covid_data <- read.csv("data/usa_county_wise.csv")

##########################################################
# Data preparation
##########################################################

# Ensure the Date column is in Date format
covid_data$Date <- as.Date(covid_data$Date, format = "%m/%d/%y")

# Split the data into training and testing sets
covid_train <- covid_data[covid_data$Date <= as.Date("2020-06-30"), ]
covid_test <- covid_data[covid_data$Date > as.Date("2020-06-30"), ]

# Remove the original dataset to free up memory
rm(covid_data)

# Force garbage collection to reclaim memory
gc()

# Display the structure of the split datasets
str(covid_train)
str(covid_test)

##########################################################
# Creating lag features and handling empty Admin2 values
##########################################################

# Function to create lag features and handle empty Admin2 values
prepare_data <- function(data) {
  data %>%
    mutate(Admin2 = replace(Admin2, Admin2 == "", "Empty")) %>% # Replace empty Admin2 with "Empty"
    arrange(Admin2, Date) %>% # Sort by Admin2 and Date
    group_by(Admin2) %>%
    mutate(
      Confirmed_lag2 = ifelse(is.na(lag(Confirmed, 2)), 0, lag(Confirmed, 2)),
      Confirmed_lag7 = ifelse(is.na(lag(Confirmed, 7)), 0, lag(Confirmed, 7)),
      Confirmed_lag14 = ifelse(is.na(lag(Confirmed, 14)), 0, lag(Confirmed, 14))
    ) %>%
    ungroup()
}

# Apply the function to the training and testing datasets
covid_train <- prepare_data(covid_train)
covid_test <- prepare_data(covid_test)

# Display the structure to verify the changes
str(covid_train)
str(covid_test)

##########################################################
# Calculating rolling averages
##########################################################

# Calculate rolling averages for the training set
covid_train <- covid_train %>%
  group_by(Admin2) %>%
  arrange(Admin2, Date, .by_group = TRUE) %>%
  mutate(
    RollingAvg7 = zoo::rollapply(Confirmed, 7, mean, fill = 0, align = "right"),
    RollingAvg14 = zoo::rollapply(Confirmed, 14, mean, fill = 0, align = "right"),
    RollingAvg30 = zoo::rollapply(Confirmed, 30, mean, fill = 0, align = "right")
  ) %>%
  ungroup()

# Calculate rolling averages for the testing set
covid_test <- covid_test %>%
  group_by(Admin2) %>%
  arrange(Admin2, Date, .by_group = TRUE) %>%
  mutate(
    RollingAvg7 = zoo::rollapply(Confirmed, 7, mean, fill = 0, align = "right"),
    RollingAvg14 = zoo::rollapply(Confirmed, 14, mean, fill = 0, align = "right"),
    RollingAvg30 = zoo::rollapply(Confirmed, 30, mean, fill = 0, align = "right")
  ) %>%
  ungroup()

##########################################################
# Calculating growth rates
##########################################################

# Making sure that there are no Inf or -Inf values generated
calculate_growth_rate <- function(current, previous) {
  if (is.na(current) || is.na(previous) || previous == 0) {
    return(0)
  } else {
    return((current - previous) / previous)
  }
}

# Calculate growth rates for the training set
covid_train <- covid_train %>%
  group_by(Admin2) %>%
  arrange(Admin2, Date, .by_group = TRUE) %>%
  mutate(
    GrowthRate1 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 1)),
    GrowthRate7 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 7)),
    GrowthRate14 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 14)),
    GrowthRate30 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 30))
  ) %>%
  ungroup()

# Calculate growth rates for the testing set
covid_test <- covid_test %>%
  group_by(Admin2) %>%
  arrange(Admin2, Date, .by_group = TRUE) %>%
  mutate(
    GrowthRate1 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 1)),
    GrowthRate7 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 7)),
    GrowthRate14 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 14)),
    GrowthRate30 = mapply(calculate_growth_rate, Confirmed, lag(Confirmed, 30))
  ) %>%
  ungroup()

##########################################################
# Checking for NA, NaN, Inf, -Inf values
##########################################################

# Function to summarize NA, NaN, Inf, and -Inf values in each column
summarize_na_nan_inf <- function(data) {
  summary_data <- data.frame(
    Column = character(),
    NA_Count = integer(),
    NaN_Count = integer(),
    Inf_Count = integer(),
    Neg_Inf_Count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (column in colnames(data)) {
    na_count <- sum(is.na(data[[column]]))
    nan_count <- sum(is.nan(data[[column]]))
    inf_count <- sum(data[[column]] == Inf)
    neg_inf_count <- sum(data[[column]] == -Inf)
    
    summary_data <- rbind(summary_data, data.frame(
      Column = column,
      NA_Count = na_count,
      NaN_Count = nan_count,
      Inf_Count = inf_count,
      Neg_Inf_Count = neg_inf_count
    ))
  }
  
  return(summary_data)
}

# Apply the function to covid_train and covid_test
train_summary <- summarize_na_nan_inf(covid_train)
test_summary <- summarize_na_nan_inf(covid_test)

##########################################################
# Print the train summary
##########################################################

print(train_summary)

##########################################################
# Print the test summary
##########################################################

print(test_summary)

##########################################################
# Handling NA and NaN values
##########################################################

covid_train$FIPS[is.na(covid_train$FIPS)] <- 0
covid_test$FIPS[is.na(covid_test$FIPS)] <- 0

##########################################################
# Filtering for continental US
##########################################################

continental_us_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                           "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                           "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                           "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                           "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                           "New Hampshire", "New Jersey", "New Mexico", "New York", 
                           "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                           "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                           "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                           "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")

covid_train <- covid_train %>% filter(Province_State %in% continental_us_states)
covid_test <- covid_test %>% filter(Province_State %in% continental_us_states)

## Data Graphing

##########################################################
# Graphing cases
##########################################################

# Convert the Date column to a Date object
covid_train$Date <- as.Date(covid_train$Date, format = "%m/%d/%y")

# Summing the total cases per day
total_cases_per_day <- covid_train %>%
  group_by(Date) %>%
  summarise(Total_Cases = sum(Confirmed, na.rm = TRUE))  # Total cases per day

# Creating the plot for daily total cases
plot <- ggplot(total_cases_per_day, aes(x = Date, y = Total_Cases)) +
  geom_line() +  # Plot the total cases as a line
  labs(title = "Total COVID-19 Cases in the US Over Time",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

# Displaying the plot
print(plot)

##########################################################
# Graphing deaths
##########################################################

# Convert the Date column to a Date object
covid_train$Date <- as.Date(covid_train$Date, format = "%m/%d/%y")

# Summing the total deaths per day
total_deaths_per_day <- covid_train %>%
  group_by(Date) %>%
  summarise(Total_Deaths = sum(Confirmed, na.rm = TRUE))  # Total deaths per day

# Creating the plot for daily total deaths
plot <- ggplot(total_deaths_per_day, aes(x = Date, y = Total_Deaths)) +
  geom_line() +  # Plot the total deaths as a line
  labs(title = "Total COVID-19 Deaths in the US Over Time",
       x = "Date",
       y = "Total Deaths") +
  theme_minimal()

# Displaying the plot
print(plot)

##########################################################
# Graphing lag features
##########################################################

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# 2-Day Lag
ggplot(covid_train, aes(x = Date, y = Confirmed_lag2)) +
  geom_line() +
  labs(title = "2-Day Lag of Confirmed Cases", x = "Date", y = "Confirmed Cases (2-Day Lag)") +
  theme_minimal()

# 7-Day Lag
ggplot(covid_train, aes(x = Date, y = Confirmed_lag7)) +
  geom_line() +
  labs(title = "7-Day Lag of Confirmed Cases", x = "Date", y = "Confirmed Cases (7-Day Lag)") +
  theme_minimal()

# 14-Day Lag
ggplot(covid_train, aes(x = Date, y = Confirmed_lag14)) +
  geom_line() +
  labs(title = "14-Day Lag of Confirmed Cases", x = "Date", y = "Confirmed Cases (14-Day Lag)") +
  theme_minimal()

par(mfrow = c(1, 1))

##########################################################
# Graphing rolling averages
##########################################################

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# 7-Day Rolling Average
ggplot(covid_train, aes(x = Date, y = RollingAvg7)) +
  geom_line() +
  labs(title = "7-Day Rolling Average of Confirmed Cases", x = "Date", y = "Confirmed Cases (7-Day Average)") +
  theme_minimal()

# 14-Day Rolling Average
ggplot(covid_train, aes(x = Date, y = RollingAvg14)) +
  geom_line() +
  labs(title = "14-Day Rolling Average of Confirmed Cases", x = "Date", y = "Confirmed Cases (14-Day Average)") +
  theme_minimal()

# 30-Day Rolling Average
ggplot(covid_train, aes(x = Date, y = RollingAvg30)) +
  geom_line() +
  labs(title = "30-Day Rolling Average of Confirmed Cases", x = "Date", y = "Confirmed Cases (30-Day Average)") +
  theme_minimal()

par(mfrow = c(1, 1))

##########################################################
# Graphing growth rates
##########################################################

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# 1-Day Growth Rate
ggplot(covid_train, aes(x = Date, y = GrowthRate1)) +
  geom_line() +
  labs(title = "Daily Growth Rate of Confirmed Cases", x = "Date", y = "Growth Rate (1-Day)") +
  theme_minimal()

# 7-Day Growth Rate
ggplot(covid_train, aes(x = Date, y = GrowthRate7)) +
  geom_line() +
  labs(title = "7-Day Growth Rate of Confirmed Cases", x = "Date", y = "Growth Rate (7-Day)") +
  theme_minimal()

# 14-Day Growth Rate
ggplot(covid_train, aes(x = Date, y = GrowthRate14)) +
  geom_line() +
  labs(title = "14-Day Growth Rate of Confirmed Cases", x = "Date", y = "Growth Rate (14-Day)") +
  theme_minimal()

# 30-Day Growth Rate
ggplot(covid_train, aes(x = Date, y = GrowthRate30)) +
  geom_line() +
  labs(title = "30-Day Growth Rate of Confirmed Cases", x = "Date", y = "Growth Rate (30-Day)") +
  theme_minimal()

par(mfrow = c(1, 1))

## Preparing feature and target sets for training

##########################################################
# One-hot encoding
##########################################################

# Ensure Province_State is a factor with consistent levels in both datasets
unique_states <- unique(c(covid_train$Province_State, covid_test$Province_State))
covid_train$Province_State <- factor(covid_train$Province_State, levels = unique_states)
covid_test$Province_State <- factor(covid_test$Province_State, levels = unique_states)

# Creating dummy variables for the training set
OHE_PS_train <- model.matrix(~ Province_State - 1, data = covid_train)
covid_train <- cbind(covid_train, OHE_PS_train)

# Creating dummy variables for the testing set
OHE_PS_test <- model.matrix(~ Province_State - 1, data = covid_test)
covid_test <- cbind(covid_test, OHE_PS_test)

##########################################################
# Prepare feature sets
##########################################################
# Set 1: Base dataset with no added features
X_train_1 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -RollingAvg7, -RollingAvg14, -RollingAvg30, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))
X_test_1 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -RollingAvg7, -RollingAvg14, -RollingAvg30, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))

# Set 2: Including the lag feature
X_train_2 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -RollingAvg7, -RollingAvg14, -RollingAvg30, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))
X_test_2 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -RollingAvg7, -RollingAvg14, -RollingAvg30, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))

# Set 3: Including the rolling average feature
X_train_3 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))
X_test_3 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))

# Set 4: Including the growth rate feature
X_train_4 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -RollingAvg7, -RollingAvg14, -RollingAvg30))
X_test_4 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14, -RollingAvg7, -RollingAvg14, -RollingAvg30))

# Set 5: Including the lag and rolling average features
X_train_5 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))
X_test_5 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -GrowthRate1, -GrowthRate7, -GrowthRate14, -GrowthRate30))

# Set 6: Including the lag and growth rate features
X_train_6 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -RollingAvg7, -RollingAvg14, -RollingAvg30))
X_test_6 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -RollingAvg7, -RollingAvg14, -RollingAvg30))

# Set 7: Including the rolling average and growth rate features
X_train_7 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14))
X_test_7 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2, -Confirmed_lag2, -Confirmed_lag7, -Confirmed_lag14))

# Set 8: Including the lag, rolling average, and growth rate features
X_train_8 <- as.matrix(covid_train %>% select(-Confirmed, -FIPS, -Admin2))
X_test_8 <- as.matrix(covid_test %>% select(-Confirmed, -FIPS, -Admin2))

##########################################################
# Prepare target variables
##########################################################

# Setting target variable for all sets
y_train <- covid_train$Confirmed
y_test <- covid_test$Confirmed

# Function to check if certain features are present in each set
check_features <- function(dataset, features) {
  feature_presence <- sapply(features, function(feature) {
    sum(grepl(feature, colnames(dataset))) > 0
  })
  names(feature_presence) <- features
  return(feature_presence)
}

# Define the feature patterns to look for
features_to_check <- c("Confirmed_lag", "RollingAvg", "GrowthRate")

# Initialize a data frame to store the presence of features in each set
feature_presence_results <- data.frame(
  Set = character(),
  Includes_Lag = logical(),
  Includes_Rolling_Avg = logical(),
  Includes_Growth_Rate = logical(),
  stringsAsFactors = FALSE
)

# Check the feature presence for each set
for (i in 1:8) {
  X_train_var <- get(paste0("X_train_", i))
  X_test_var <- get(paste0("X_test_", i))
  
  # Check features in training set
  train_features_present <- check_features(X_train_var, features_to_check)
  
  # Check features in test set
  test_features_present <- check_features(X_test_var, features_to_check)
  
  # Assuming the same features are present in both train and test sets, we can just use train_features_present
  feature_presence_results <- rbind(feature_presence_results, data.frame(
    Set = paste0("Set ", i),
    Includes_Lag = train_features_present["Confirmed_lag"],
    Includes_Rolling_Avg = train_features_present["RollingAvg"],
    Includes_Growth_Rate = train_features_present["GrowthRate"]
  ))
}

##########################################################
# Printing the feature table
##########################################################

# Reset row names to ensure they are sequential
row.names(feature_presence_results) <- NULL

# Print the table of feature presence results
print(feature_presence_results)

## Training and Testing LightGBM and Elastic Net Models

##########################################################
# Running LightGBM model
##########################################################
# Initialize a data frame to store the RMSE results
rmse_results <- data.frame(
  Set = paste0("Set ", 1:8),
  LightGBM = rep(NA, 8), # Placeholder vector of NAs
  ElasticNet = rep(NA, 8), # Placeholder vector of NAs
  stringsAsFactors = FALSE
)

# Iterate over each feature set
for (i in 1:8) {
  # Dynamically construct the training and testing set variable names
  X_train_var <- get(paste0("X_train_", i))
  X_test_var <- get(paste0("X_test_", i))
  
  # Convert data to LightGBM format
  train_data <- lgb.Dataset(data = X_train_var, label = y_train)
  test_data <- lgb.Dataset(data = X_test_var, label = y_test, reference = train_data)
  
  # Set parameters for the model
  params <- list(
    objective = "regression",
    metric = "rmse",
    num_leaves = 31,
    learning_rate = 0.05,
    n_estimators = 100
  )
  
  # Train the model
  lgb_model <- lgb.train(
    params = params,
    data = train_data,
    nrounds = 100,
    valids = list(test = test_data),
    early_stopping_rounds = 10,
    verbose = -1
  )
  
  # Make predictions and calculate RMSE for the test set
  predictions_lgb <- predict(lgb_model, X_test_var)
  rmse_lgbm <- sqrt(mean((predictions_lgb - y_test)^2))
  
  # Add the results to the data frame
  rmse_results$LightGBM[i] <- rmse_lgbm
}

##########################################################
# Running Elastic Net model
##########################################################

# Iterate over each feature set
for (i in 1:8) {
  # Dynamically construct the training and testing set variable names
  X_train_var <- get(paste0("X_train_", i))
  X_test_var <- get(paste0("X_test_", i))
  
  ##########################################################
  # Elastic Net Model
  ##########################################################
  # Prepare matrix as glmnet requires a matrix
  X_train_matrix <- X_train_var
  X_test_matrix <- X_test_var
  
  # Alpha = 0.5 for elastic net (0 for ridge, 1 for lasso)
  alpha_value <- 0.5
  
  # Fit the model
  cv_model <- cv.glmnet(X_train_matrix, y_train, alpha = alpha_value, family = "gaussian")
  
  # Best lambda value
  best_lambda <- cv_model$lambda.min
  
  # Refit the model with the best lambda value
  final_model <- glmnet(X_train_matrix, y_train, alpha = alpha_value, lambda = best_lambda, family = "gaussian")
  
  # Predict on test set
  predictions_enet <- predict(final_model, s = best_lambda, newx = X_test_matrix)
  
  # Calculate RMSE for Elastic Net
  rmse_enet <- sqrt(mean((y_test - predictions_enet)^2))
  
  # Add the results to the data frame
  rmse_results$ElasticNet[i] <- rmse_enet
}

## Results

##########################################################
# Normalizing and comparing models
##########################################################

# Calculate the maximum and minimum confirmed cases to find the range for normalization
max_confirmed <- max(c(covid_train$Confirmed, covid_test$Confirmed))
min_confirmed <- min(c(covid_train$Confirmed, covid_test$Confirmed))
range_confirmed <- max_confirmed - min_confirmed

# Normalize RMSE values for both models and add them to the rmse_results data frame
rmse_results$NRMSE_LightGBM <- rmse_results$LightGBM / range_confirmed
rmse_results$NRMSE_ElasticNet <- rmse_results$ElasticNet / range_confirmed

# Print the updated table of RMSE results with normalized values
print(rmse_results)

# Determine the best performing model and set
combined_nrmse <- c(rmse_results$NRMSE_LightGBM, rmse_results$NRMSE_ElasticNet)
best_model_index <- which.min(combined_nrmse)
best_model_nrmse <- min(combined_nrmse)
best_model_type <- ifelse(best_model_index <= nrow(rmse_results), "LightGBM", "ElasticNet")
best_model_set <- ifelse(best_model_index <= nrow(rmse_results), best_model_index, best_model_index - nrow(rmse_results))

# Output the statement on the best model and set
cat("The absolute best-performing model is", best_model_type, "using feature Set", best_model_set, "with a NRMSE of", best_model_nrmse, "\n")