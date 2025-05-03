# Load necessary libraries
library(readr)
library(lubridate)
library(scales)
library(ggplot2)
library(ggfortify)
library(dplyr)

# Load data
df <- read_csv("C:/Users/0761343532/Documents/A_R_curs/data_insamling_volkswagen_v2.csv", locale = locale(encoding = "UTF-8"))

summary(df)
head(df)

#Data Cleaning

# Remove empty columns
df <- df %>% dplyr::select(-starts_with("Unnamed"))
df <- df[, 1:14]

str(df)

#Drop columns not useful for regression
#-------------------------------------------------------------------------------
df <- df %>% select(-c( date_in_traffic, colour, brand, region))

str(df)

# Clean numeric columns: remove spaces/commas and convert
# ---------------------------------------------------------------
df <- df %>%
  mutate(
    price = as.numeric(gsub("[^0-9]", "", price)),
    mile = as.numeric(gsub("[^0-9]", "", mile)),
    model_year = as.numeric(gsub("[^0-9]", "", model_year)),
    horsepower = as.numeric(gsub("[^0-9]", "", horsepower))
  )

# Drop rows with NA in any numerik columns
model_vars <- c("price", "mile", "horsepower", "model_year")
df <- df %>% filter(if_all(all_of(model_vars), ~ !is.na(.)))
head(df)
str(df)


# Check and visualise price rage
#-------------------------------------------------------------------------------
# Calculate the price range (min to max)
price_range <- range(df$price, na.rm = TRUE)
print(price_range)

# Box plot of the prices
#-------------------------------------------------------------------------------


ggplot(df, aes(y = price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Car Prices",
       y = "Price (SEK)") +
  scale_y_continuous(
    labels = label_comma(),
    breaks = seq(0, max(df$price, na.rm = TRUE), by = 100000)
  ) +
  theme_minimal()

# Take out rows ao price outliers
#-------------------------------------------------------------------------------
df <- df %>% filter(price <= 900000)

# Histogram with X-axis formatted, limited to 750,000 and step size of 100,000
#-------------------------------------------------------------------------------
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 10000, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Car Prices",
    x = "Price (SEK)",
    y = "Number of Cars"
  ) +
  scale_x_continuous(
    labels = label_comma(),                  # Format with comma separator
    limits = c(0, 750000),                   # Set axis limit
    breaks = seq(0, 750000, by = 100000)     # Set tick marks every 100,000
  ) +
  theme_minimal()
str(df)

#Clean  model_year column
#-------------------------------------------------------------------------------
# Count model_year values
model_year_counts <- df %>%
  group_by(model_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Print the number of occurrences for each model year
print(as.data.frame(table(df$model_year)))

# Check structure and NA values
str(df)
sum(is.na(df$model_year))

# Bar chart: Number of cars by model year with labels
ggplot(model_year_counts, aes(x = factor(model_year), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = count), vjust = -0.5, size = 3.5) +  # <- Add labels above bars
  labs(title = "Number of Cars by Model Year",
       x = "Model Year",
       y = "Number of Cars") +
  scale_y_continuous(
    labels = label_comma(),
    breaks = seq(0, max(model_year_counts$count) + 50, by = 100)  # step size
  ) +
  theme_minimal()

# Filter to keep only years between 2010 and 2024
df <- df %>% filter(model_year >= 2010 & model_year <= 2024)

# Using model_year to count car age
#-------------------------------------------------------------------------------
df$car_age <- as.numeric(format(Sys.Date(), "%Y")) - df$model_year
df <- df %>%
  mutate(
    car_age = as.numeric(car_age)
  )
sum(is.na(df$car_age))

#Drop model_year column
#-------------------------------------------------------------------------------
df <- df %>% select(-c(model_year))

str(df)

# Clean seller column
#-------------------------------------------------------------------------------
df$seller <- tolower(df$seller)  # Convert to lowercase
df <- df %>% filter(!is.na(seller) & seller != "")  # Remove missing/empty values

# Count seller types
seller_counts <- df %>%
  group_by(seller) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the number of occurrences for each seller type
print(as.data.frame(table(df$seller)))


str(df)

# Clean fuel column 
#-------------------------------------------------------------------------------

df$fuel <- tolower(df$fuel)  # Convert to lowercase
df <- df %>% filter(!is.na(fuel) & fuel != "")  # Remove missing/empty values

df$fuel <- df$fuel %>%
  recode(
    "disel" = "diesel",
    "miljöbränsle" = "miljöbränsle/hybrid"
  )
# Visualisation

# Skapa data frame med antal (antal bränsletyper)
fuel_counts <- df %>%
  count(fuel) %>%
  arrange(desc(n))

# Rita stapeldiagram med datalabels för bränsletyper
ggplot(fuel_counts, aes(x = reorder(fuel, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +  # Lägg till etiketter
  labs(
    title = "Antal Bilar per Bränsletyp",
    x = "Bränsletyp",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the number of occurrences for each fuel type
print(as.data.frame(table(df$fuel)))
str(df)

#clean gearbox column
#-------------------------------------------------------------------------------
df$gearbox <- tolower(df$gearbox)  # Convert to lowercase
df <- df %>% filter(!is.na(gearbox) & gearbox != "")  # Remove missing/empty values

# Count gearbox types
  gearbox_counts <- df %>%
  group_by(gearbox) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the number of occurrences for each gearbox type
print(as.data.frame(table(df$gearbox)))

str(df)

# Clean car_type column
#-------------------------------------------------------------------------------
# Clean car_type column
df$car_type <- tolower(df$car_type) # Convert to lowercase
df <- df %>% filter(!is.na(car_type) & car_type != "") # Remove missing/empty values
df$car_type <- trimws(df$car_type) # Trim whitespace 

# Fix typos in car_type
df$car_type <- df$car_type %>%
  recode("halvkomni" = "halvkombi")

# Count all car types first
car_type_counts <- df %>%
  count(car_type) %>%
  arrange(desc(n))
# Bar chart with data labels (before cleaning)
ggplot(car_type_counts, aes(x = reorder(car_type, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Antal bilar per biltyp",
    x = "Biltyp",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

str(df)

# Clean drive column
#-------------------------------------------------------------------------------
df$drive <- tolower(df$drive)  # Convert to lowercase
df <- df %>% filter(!is.na(drive) & drive != "")  # Remove missing/empty values

# Trim whitespace
df$drive <- trimws(df$drive)

# Count drive types
drive_counts <- df %>%
  group_by(drive) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the number of occurrences for each drive type
print(as.data.frame(table(df$drive)))

# Cleaning Model Column
#-----------------------------------------------------------------------
# Standardize model names
df$model <- tolower(df$model)
# Skapa data frame med antal (antal bilmodeller efter rensning)
model_counts <- df %>%
  count(model) %>%
  arrange(desc(n))

# Rita stapeldiagram med datalabels för bilmodeller
ggplot(model_counts, aes(x = reorder(model, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Använd Violet färg
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +  # Lägg till etiketter
  labs(
    title = "Antal Bilar per Modell",
    x = "Bilmodell",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Count the number of occurrences for each car model
model_counts <- df %>%
  group_by(model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the number of occurrences for each car model

print(as.data.frame(table(df$model)))

# Count how many times each model appears
model_freq <- table(df$model)

# Identify models with less than 10 occurrences
rare_models <- names(model_freq[model_freq < 10])


#  Filter and count how many cars belong to those rare models
rare_model_cars <- df %>% filter(model %in% rare_models)
nrow(rare_model_cars)  # This is the total number of such cars

# Remove rows that belong to rare models
df <- df %>% filter(!(model %in% rare_models))

# Print the remaining model frequencies
print(as.data.frame(table(df$model)))
str(df)

#Car count
#-------------------------------------------------------------------------------
#Extra !!!
# Count how many cars are above 650,000 SEK
cars_above_650k <- df %>%
  filter(price > 650000) %>%
  nrow()

# Print result
print(cars_above_650k)

# Remove outliers (e.g., outside normal price range)
df <- df %>%
  filter(price > 30000 & price < 650000)

# Check structure
str(df)


# Split the dataset to train/validation/test
# --------------------------------------------------------------------------------------

set.seed(123)  # for reproducibility
n <- nrow(df)

# 60% Train
train_index <- sample(1:n, size = 0.6 * n)
temp <- df[-train_index, ]

# 20% Validation, 20% Test from remaining
val_index <- sample(1:nrow(temp), size = 0.5 * nrow(temp))

train <- df[train_index, ]
validation <- temp[val_index, ]
test <- temp[-val_index, ]

#Built the multiple lenear regression model
# ----------------------------------------------------------------------------------------------

# Fit a multiple linear regression model
model_initial <- lm(price ~ mile + horsepower + car_age + fuel + gearbox + drive, data = train)

# Summary of the model
summary(model_initial)

#vif(model_initial)
#-------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(model_initial)
#-------------------------------------------------------------------------------
# Modified model for train and validation set
#-------------------------------------------------------------------------------

# train: Create binary indicators for selected categorical values
train <- train %>%
  mutate(
    fuel_diesel = ifelse(fuel == "diesel", 1, 0),
    gearbox_manuell = ifelse(gearbox == "manuell", 1, 0),
    drive_tvahjulsdriven = ifelse(drive == "tvåhjulsdriven", 1, 0)
  )

# validation: Create binary indicators for selected categorical values
validation <- validation %>%
  mutate(
    fuel_diesel = ifelse(fuel == "diesel", 1, 0),
    gearbox_manuell = ifelse(gearbox == "manuell", 1, 0),
    drive_tvahjulsdriven = ifelse(drive == "tvåhjulsdriven", 1, 0)
  )

# fit the simplified model
#-------------------------------------------------------------------------------

model_simplified <- lm(price ~ mile + horsepower + car_age + fuel_diesel + gearbox_manuell+drive_tvahjulsdriven, data = train)

summary(model_simplified)

par(mfrow = c(2, 2))
plot(model_simplified)


# Identify influentional rows
influence <- cooks.distance(model_simplified)
influential_rows <- which(influence > 4 / length(influence))

#Review rows
View(train[influential_rows, ])  # Or print(train[influential_rows, ])
#Remove them from train
train <- train[-influential_rows, ]

#Retrain the model
model_simplified <- lm(price ~ mile + horsepower + car_age + fuel_diesel + gearbox_manuell+drive_tvahjulsdriven, data = train)

summary(model_simplified)

par(mfrow = c(2, 2))
plot(model_simplified)

library(car) 
vif(model_simplified)
vif(model_initial)

# Predict using initial model 
pred_initial <- predict(model_initial, newdata = validation)

# Predict using simplified model
pred_simplified <- predict(model_simplified, newdata = validation)

# Function to evaluate model performance
evaluate_model <- function(actual, predicted) {
  SSE <- sum((actual - predicted)^2)
  SST <- sum((actual - mean(actual))^2)
  rsq <- 1 - SSE/SST
  rmse <- sqrt(mean((actual - predicted)^2))
  cat("R²:", round(rsq, 3), "\n")
  cat("RMSE:", round(rmse, 0), "\n")
}

# Compare both models
cat("🔹 Initial Model (model_initial):\n")
evaluate_model(validation$price, pred_initial)

cat("\n🔹 Simplified Model (model_simplified):\n")
evaluate_model(validation$price, pred_simplified)


summary(validation$price)
summary(pred_initial)
summary(pred_simplified)

#Log-transformerad modell 
#-------------------------------------------------------------------------------


# Visualisering av log-transformation på bil priser
#-------------------------------------------------------------------------------
train$log_price <- log(train$price)

# Histogram + densitetskurva
ggplot(train, aes(x = log_price)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  labs(
    title = "Fördelning av log-transformerade bilpriser",
    x = "log(Pris)",
    y = "Täthet (Density)"
  ) +
  theme_minimal()
#-------------------------------------------------------------------------------
# Log-transformerad modell (based at model_simplified)
model_simplified_log <- lm(log(price) ~ mile + horsepower + car_age + fuel_diesel + gearbox_manuell + drive_tvahjulsdriven,data = train)

# Modellöversikt
summary(model_simplified_log)

# Visualisering: diagnostikplottar
par(mfrow = c(2, 2))
plot(model_simplified_log)

# Prediktion på valideringsdata
validation$predicted_log <- predict(model_simplified_log, newdata = validation)
validation$predicted_price <- exp(validation$predicted_log)  # Återtransformera till originalskala

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((validation$price - validation$predicted_price)^2))

# R² (på originalskalan)
ss_total <- sum((validation$price - mean(validation$price))^2)
ss_residual <- sum((validation$price - validation$predicted_price)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Skriv ut resultat
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "SEK\n")
cat("R² (Valideringsdata):", round(r_squared, 4), "\n")

#-------------------------------------------------------------------------------
# Kolla multikollinearitet

vif(model_simplified_log)

# Skapa binära indikatorer i testdata
test <- test %>%
  mutate(
    fuel_diesel = ifelse(fuel == "diesel", 1, 0),
    gearbox_manuell = ifelse(gearbox == "manuell", 1, 0),
    drive_tvahjulsdriven = ifelse(drive == "tvåhjulsdriven", 1, 0)
  )

# Prediction best model using model_simplified_log
#-------------------------------------------------------------------------------
# Prediktion på testdata
test$predicted_log <- predict(model_simplified_log, newdata = test)
test$predicted_price <- exp(test$predicted_log)  # Återtransformera till prisnivå

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((test$price - test$predicted_price)^2))

# R² manuellt (eftersom modellen är log-transformerad)
ss_total <- sum((test$price - mean(test$price))^2)
ss_residual <- sum((test$price - test$predicted_price)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Skriv ut resultat
cat("RMSE (Testdata):", round(rmse, 2), "SEK\n")
cat("R² (Testdata):", round(r_squared, 4), "\n")

#saving the model
saveRDS(model_simplified_log, "basta_modell.rds")
#Testing Full model
#-------------------------------------------------------------------------------
model_full <- lm(price ~ .-model, data = train)

# Summary of the model
summary(model_full)

vif(model_full)

alias(model_full)
#-------------------------------------------------------------------------------

#Choosing the best parameters
#-------------------------------------------------------------------------------
library(leaps)  
library(Metrics)

model_best_p <- regsubsets(price ~ .-model, data = train, nvmax = 16)
model_best_p_summary = summary(model_best_p)
model_best_p_summary


names(model_best_p_summary)
par(mfrow = c(1, 1))
plot(model_best_p_summary$adjr2)
model_best_p_summary$adjr2

# Testing the model with best parameters
#-------------------------------------------------------------------------------
model_new <- lm(price ~ mile + horsepower + car_age + fuel_diesel, data = train)

summary(model_new)

vif(model_new)
#diagnostiska grafer

par(mfrow = c(2, 2))
plot(model_new)



