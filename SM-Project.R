
install.packages("tidyverse", dependencies = TRUE) # data manipulation and plotting
install.packages("readr", dependencies = TRUE) # reading CSV files
install.packages("dplyr", dependencies = TRUE) # data wrangling
install.packages("evaluate")
install.packages("skimr", dependencies = TRUE) # Summarize the dataset
install.packages("ggplot2", dependencies = TRUE) # advanced graphs
install.packages("gridExtra", dependencies = TRUE) # arrange multiple plots
install.packages("caret", dependencies = TRUE) # model evaluation
install.packages("pROC", dependencies = TRUE) # Calculate and plot the ROC curve
install.packages("glmnet", dependencies = TRUE) # Ridge and LASSO regression

library(tidyverse)
library(readr)
library(skimr)
library(dplyr)

hotel <- read_csv("C:/Users/dhanu/Desktop/Hotel/hotel_bookings.csv")


dim(hotel) # How many rows and columns
head(hotel) # See first 6 rows
colnames(hotel) # See all variable names
str(hotel) # Check variable types and structure
summary(hotel) # Basic summary statistics
colSums(is.na(hotel)) # Check missing values in each column
skim(hotel) # Quick overall skim


hotel$children[is.na(hotel$children)] <- 0 # Replace the 4 missing children values with 0
colSums(is.na(hotel))

hotel <- hotel %>%
  mutate(
    # Total nights stayed
    total_nights = stays_in_weekend_nights + stays_in_week_nights,
    
    # Total guests
    total_guests = adults + children + babies,
    
    # Convert month name to number for ordering
    arrival_month_num = match(arrival_date_month, month.name),
    
    # Convert cancellation to factor (category)
    is_canceled = as.factor(is_canceled),
    
    # Convert hotel type to factor
    hotel = as.factor(hotel)
  )

str(hotel)

#Task 3-Descriptive Analysis
#Block 1 — Basic Descriptive Statistics
library(skimr)

skim(hotel) # Overall summary
table(hotel$hotel) # Hotel type distribution
table(hotel$is_canceled) # Cancellation rate
prop.table(table(hotel$is_canceled)) * 100

#Block 2 — Visualizations
library(ggplot2)
library(gridExtra)

# Plot 1 - Hotel type distribution
p1 <- ggplot(hotel, aes(x = hotel, fill = hotel)) +
  geom_bar() +
  labs(title = "Distribution of Hotel Types",
       x = "Hotel Type", y = "Number of Bookings") +
  theme_minimal()

# Plot 2 - Cancellation rate
p2 <- ggplot(hotel, aes(x = is_canceled, fill = is_canceled)) +
  geom_bar() +
  scale_x_discrete(labels = c("Not Cancelled", "Cancelled")) +
  labs(title = "Booking Cancellation Status",
       x = "Status", y = "Number of Bookings") +
  theme_minimal()

# Plot 3 - Bookings by month
p3 <- ggplot(hotel, aes(x = arrival_month_num, fill = hotel)) +
  geom_bar(position = "dodge") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                "Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title = "Bookings by Month",
       x = "Month", y = "Number of Bookings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4 - Average Daily Rate distribution
p4 <- ggplot(hotel, aes(x = adr, fill = hotel)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  labs(title = "Average Daily Rate (ADR) Distribution",
       x = "ADR (€)", y = "Frequency") +
  xlim(0, 500) +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2) # Display all plots together

#Block 3 — More Business Insights
# Average Daily Rate by hotel type
hotel %>%
  group_by(hotel) %>%
  summarise(
    avg_adr = mean(adr, na.rm = TRUE),
    median_adr = median(adr, na.rm = TRUE),
    avg_nights = mean(total_nights, na.rm = TRUE),
    avg_lead_time = mean(lead_time, na.rm = TRUE),
    cancellation_rate = mean(is_canceled == 1) * 100
  )

# Top 10 countries by bookings
hotel %>%
  count(country, sort = TRUE) %>%
  head(10)

# Bookings by market segment
hotel %>%
  count(market_segment, sort = TRUE)

# Plot 5 - Cancellation rate by hotel type
p5 <- hotel %>%
  group_by(hotel, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100) %>%
  ggplot(aes(x = hotel, y = pct, fill = is_canceled)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("Not Cancelled", "Cancelled")) +
  labs(title = "Cancellation Rate by Hotel Type",
       x = "Hotel Type", y = "Percentage (%)",
       fill = "Status") +
  theme_minimal()

# Plot 6 - Average ADR by month
p6 <- hotel %>%
  group_by(arrival_month_num, hotel) %>%
  summarise(avg_adr = mean(adr)) %>%
  ggplot(aes(x = arrival_month_num, y = avg_adr, color = hotel, group = hotel)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Feb","Mar","Apr","May",
                                "Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  labs(title = "Average Daily Rate by Month",
       x = "Month", y = "Average ADR (€)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 7 - Lead time distribution
p7 <- ggplot(hotel, aes(x = lead_time, fill = hotel)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  labs(title = "Lead Time Distribution",
       x = "Lead Time (Days)", y = "Frequency") +
  xlim(0, 400) +
  theme_minimal()

# Plot 8 - Total nights stayed
p8 <- hotel %>%
  filter(total_nights <= 14) %>%
  ggplot(aes(x = total_nights, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Length of Stay Distribution",
       x = "Total Nights", y = "Number of Bookings") +
  theme_minimal()

grid.arrange(p5, p6, p7, p8, ncol = 2)

#Task 4-Statistical Inference
#Test 1 - t-test

# Separate ADR for each hotel type
city_adr <- hotel$adr[hotel$hotel == "City Hotel"]
resort_adr <- hotel$adr[hotel$hotel == "Resort Hotel"]

# Check normality (needed to justify t-test)
shapiro_city <- shapiro.test(sample(city_adr, 5000))
shapiro_resort <- shapiro.test(sample(resort_adr, 5000))
print(shapiro_city)
print(shapiro_resort)

# Run Welch's t-test (does not assume equal variance)
ttest_result <- t.test(city_adr, resort_adr)
print(ttest_result)

#Test 2 - Chi-square

# Create contingency table
cancel_table <- table(hotel$hotel, hotel$is_canceled)
print(cancel_table)

# Run chi-square test
chi_result <- chisq.test(cancel_table)
print(chi_result)

#Test 3 - ANOVA

# Run ANOVA
anova_result <- aov(adr ~ arrival_date_month, data = hotel)
summary(anova_result)

# Post-hoc test to see WHICH months differ
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Visualize
ggplot(hotel, aes(x = reorder(arrival_date_month, arrival_month_num), 
                  y = adr, fill = arrival_date_month)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "ADR Distribution by Month (ANOVA)",
       x = "Month", y = "ADR (€)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Test 4 - F-test

# F-test for equality of variances
ftest_result <- var.test(city_adr, resort_adr)
print(ftest_result)

#Task 5-Predictive Modelling

# Prepare modelling dataset
hotel_model <- hotel %>%
  filter(adr > 0 & adr < 500) %>%  # remove extreme outliers
  mutate(
    is_canceled = as.numeric(as.character(is_canceled)),
    hotel_type = ifelse(hotel == "City Hotel", 1, 0),
    is_repeated_guest = as.numeric(is_repeated_guest),
    total_nights = stays_in_weekend_nights + stays_in_week_nights
  ) %>%
  select(
    is_canceled, hotel_type, lead_time, arrival_month_num,
    stays_in_weekend_nights, stays_in_week_nights, total_nights,
    adults, children, total_guests, is_repeated_guest,
    previous_cancellations, previous_bookings_not_canceled,
    booking_changes, days_in_waiting_list, adr,
    required_car_parking_spaces, total_of_special_requests,
    market_segment, deposit_type, customer_type
  ) %>%
  mutate(
    market_segment = as.factor(market_segment),
    deposit_type = as.factor(deposit_type),
    customer_type = as.factor(customer_type)
  ) %>%
  na.omit()

# Check dimensions
dim(hotel_model)

#Model 1 - Logistic Regression
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into 80% training and 20% testing
train_index <- createDataPartition(hotel_model$is_canceled, p = 0.8, list = FALSE)
train_data <- hotel_model[train_index, ]
test_data <- hotel_model[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Build Logistic Regression Model
logit_model <- glm(is_canceled ~ hotel_type + lead_time + arrival_month_num +
                     total_nights + adults + children +
                     is_repeated_guest + previous_cancellations +
                     previous_bookings_not_canceled + booking_changes +
                     days_in_waiting_list + required_car_parking_spaces +
                     total_of_special_requests + deposit_type +
                     market_segment + customer_type,
                   data = train_data,
                   family = binomial(link = "logit"))

# Model summary
summary(logit_model)

#Model Evaluation
# Make predictions on test data
logit_pred_prob <- predict(logit_model, newdata = test_data, type = "response")
logit_pred_class <- ifelse(logit_pred_prob > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- confusionMatrix(as.factor(logit_pred_class), 
                               as.factor(test_data$is_canceled))
print(conf_matrix)

# ROC Curve and AUC
library(pROC)

roc_curve <- roc(test_data$is_canceled, logit_pred_prob)
auc_value <- auc(roc_curve)
cat("AUC Score:", auc_value, "\n")

# Plot ROC curve
plot(roc_curve, 
     main = paste("ROC Curve - Logistic Regression (AUC =", round(auc_value, 3), ")"),
     col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "red")

#Model 2 - Multiple Linear Regression

# Build Multiple Linear Regression to predict ADR
lm_model <- lm(adr ~ hotel_type + lead_time + arrival_month_num +
                 total_nights + adults + children +
                 is_repeated_guest + previous_cancellations +
                 booking_changes + total_of_special_requests +
                 required_car_parking_spaces + days_in_waiting_list +
                 deposit_type + market_segment + customer_type,
               data = train_data)

# Model summary
summary(lm_model)

#Model Evaluation
# Predictions on test data
lm_pred <- predict(lm_model, newdata = test_data)

# Calculate performance metrics
lm_mae <- mean(abs(lm_pred - test_data$adr))
lm_rmse <- sqrt(mean((lm_pred - test_data$adr)^2))
lm_r2 <- cor(lm_pred, test_data$adr)^2

cat("Linear Regression Performance \n")
cat("MAE  (Mean Absolute Error):", round(lm_mae, 2), "\n")
cat("RMSE (Root Mean Square Error):", round(lm_rmse, 2), "\n")
cat("R-squared:", round(lm_r2, 3), "\n")

# Plot actual vs predicted
library(ggplot2)

ggplot(data.frame(Actual = test_data$adr, Predicted = lm_pred), 
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  labs(title = "Actual vs Predicted ADR - Linear Regression",
       x = "Actual ADR (€)", y = "Predicted ADR (€)") +
  xlim(0, 400) + ylim(0, 400) +
  theme_minimal()

# Check regression assumptions
par(mfrow = c(2,2))
plot(lm_model)

#Model 3 - LASSO & Ridge Regression
library(glmnet)

# Prepare matrix format required by glmnet
# For ADR prediction
x_train <- model.matrix(adr ~ hotel_type + lead_time + arrival_month_num +
                          total_nights + adults + children +
                          is_repeated_guest + previous_cancellations +
                          booking_changes + total_of_special_requests +
                          required_car_parking_spaces + days_in_waiting_list +
                          deposit_type + market_segment + customer_type,
                        data = train_data)[, -1]

y_train <- train_data$adr

x_test <- model.matrix(adr ~ hotel_type + lead_time + arrival_month_num +
                         total_nights + adults + children +
                         is_repeated_guest + previous_cancellations +
                         booking_changes + total_of_special_requests +
                         required_car_parking_spaces + days_in_waiting_list +
                         deposit_type + market_segment + customer_type,
                       data = test_data)[, -1]

y_test <- test_data$adr

# RIDGE REGRESSION (alpha = 0)
set.seed(123)
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambda_ridge <- ridge_cv$lambda.min
cat("Best Ridge Lambda:", best_lambda_ridge, "\n")

ridge_pred <- predict(ridge_cv, s = best_lambda_ridge, newx = x_test)
ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
ridge_mae <- mean(abs(ridge_pred - y_test))
ridge_r2 <- cor(as.vector(ridge_pred), y_test)^2
cat("Ridge RMSE:", round(ridge_rmse, 2), "\n")
cat("Ridge MAE:", round(ridge_mae, 2), "\n")
cat("Ridge R-squared:", round(ridge_r2, 3), "\n")

# LASSO REGRESSION (alpha = 1)
set.seed(123)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda_lasso <- lasso_cv$lambda.min
cat("Best LASSO Lambda:", best_lambda_lasso, "\n")

lasso_pred <- predict(lasso_cv, s = best_lambda_lasso, newx = x_test)
lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
lasso_mae <- mean(abs(lasso_pred - y_test))
lasso_r2 <- cor(as.vector(lasso_pred), y_test)^2
cat("LASSO RMSE:", round(lasso_rmse, 2), "\n")
cat("LASSO MAE:", round(lasso_mae, 2), "\n")
cat("LASSO R-squared:", round(lasso_r2, 3), "\n")

# Compare all three models
comparison <- data.frame(
  Model = c("Linear Regression", "Ridge Regression", "LASSO Regression"),
  RMSE = c(lm_rmse, ridge_rmse, lasso_rmse),
  MAE = c(lm_mae, ridge_mae, lasso_mae),
  R_squared = c(lm_r2, ridge_r2, lasso_r2)
)
print(comparison)

# Plot comparison
ggplot(comparison, aes(x = Model)) +
  geom_bar(aes(y = RMSE, fill = Model), stat = "identity") +
  labs(title = "Model Comparison — RMSE",
       x = "Model", y = "RMSE (€)") +
  theme_minimal() +
  theme(legend.position = "none")

#Conceptual Framework Diagram
library(ggplot2)
library(gridExtra)

# Create HRIS Framework Diagram using ggplot2
diagram <- ggplot() +
  # Background
  theme_void() +
  
  # Data Sources boxes (blue)
  annotate("rect", xmin=1, xmax=3, ymin=8.5, ymax=9.5, fill="#AED6F1", color="black") +
  annotate("rect", xmin=4, xmax=6, ymin=8.5, ymax=9.5, fill="#AED6F1", color="black") +
  annotate("rect", xmin=7, xmax=9, ymin=8.5, ymax=9.5, fill="#AED6F1", color="black") +
  
  # Data Sources labels
  annotate("text", x=2, y=9, label="Hotel Booking\nData", size=3, fontface="bold") +
  annotate("text", x=5, y=9, label="Historical\nRecords", size=3, fontface="bold") +
  annotate("text", x=8, y=9, label="Market\nData", size=3, fontface="bold") +
  
  # Processing Layer (yellow)
  annotate("rect", xmin=2, xmax=8, ymin=7, ymax=8, fill="#F9E79F", color="black") +
  annotate("text", x=5, y=7.5, label="Data Processing & Integration Layer", 
           size=3.5, fontface="bold") +
  
  # Model boxes (green)
  annotate("rect", xmin=0.5, xmax=2.5, ymin=5, ymax=6.5, fill="#A9DFBF", color="black") +
  annotate("rect", xmin=3,   xmax=5,   ymin=5, ymax=6.5, fill="#A9DFBF", color="black") +
  annotate("rect", xmin=5.5, xmax=7.5, ymin=5, ymax=6.5, fill="#A9DFBF", color="black") +
  annotate("rect", xmin=8,   xmax=10,  ymin=5, ymax=6.5, fill="#A9DFBF", color="black") +
  
  # Model labels
  annotate("text", x=1.5, y=5.75, label="Cancellation\nRisk Engine\n(Logistic Reg.)", 
           size=2.8, fontface="bold") +
  annotate("text", x=4,   y=5.75, label="Dynamic\nPricing Advisor\n(Linear Reg.)",   
           size=2.8, fontface="bold") +
  annotate("text", x=6.5, y=5.75, label="Demand\nForecasting\n(ARIMA)",              
           size=2.8, fontface="bold") +
  annotate("text", x=9,   y=5.75, label="Customer\nSegment\nIntelligence",           
           size=2.8, fontface="bold") +
  
  # Dashboard box (red)
  annotate("rect", xmin=2.5, xmax=7.5, ymin=3.2, ymax=4.5, fill="#F1948A", color="black") +
  annotate("text", x=5, y=3.85, label="HRIS Intelligence Dashboard", 
           size=4, fontface="bold") +
  
  # Output boxes (purple)
  annotate("rect", xmin=0.5, xmax=2.5, ymin=1.5, ymax=2.8, fill="#D7BDE2", color="black") +
  annotate("rect", xmin=3,   xmax=5,   ymin=1.5, ymax=2.8, fill="#D7BDE2", color="black") +
  annotate("rect", xmin=5.5, xmax=7.5, ymin=1.5, ymax=2.8, fill="#D7BDE2", color="black") +
  annotate("rect", xmin=8,   xmax=10,  ymin=1.5, ymax=2.8, fill="#D7BDE2", color="black") +
  
  # Output labels
  annotate("text", x=1.5, y=2.15, label="Cancellation\nAlerts",        size=3) +
  annotate("text", x=4,   y=2.15, label="Pricing\nRecommendations",    size=3) +
  annotate("text", x=6.5, y=2.15, label="Demand\nForecasts",           size=3) +
  annotate("text", x=9,   y=2.15, label="Marketing\nInsights",         size=3) +
  
  # Decision maker box (orange)
  annotate("rect", xmin=2.5, xmax=7.5, ymin=0, ymax=1.2, fill="#FAD7A0", color="black") +
  annotate("text", x=5, y=0.6, label="Hotel Management — Strategic Decision Making", 
           size=3.5, fontface="bold") +
  
  # Arrows (top to processing)
  annotate("segment", x=2, xend=2, y=8.5, yend=8, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=5, xend=5, y=8.5, yend=8, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=8, xend=8, y=8.5, yend=8, arrow=arrow(length=unit(0.3,"cm"))) +
  
  # Arrows (processing to models)
  annotate("segment", x=1.5, xend=1.5, y=7, yend=6.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=4,   xend=4,   y=7, yend=6.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=6.5, xend=6.5, y=7, yend=6.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=9,   xend=9,   y=7, yend=6.5, arrow=arrow(length=unit(0.3,"cm"))) +
  
  # Arrows (models to dashboard)
  annotate("segment", x=1.5, xend=3.5, y=5, yend=4.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=4,   xend=4.5, y=5, yend=4.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=6.5, xend=5.5, y=5, yend=4.5, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=9,   xend=6.5, y=5, yend=4.5, arrow=arrow(length=unit(0.3,"cm"))) +
  
  # Arrows (dashboard to outputs)
  annotate("segment", x=3.5, xend=1.5, y=3.2, yend=2.8, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=4.5, xend=4,   y=3.2, yend=2.8, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=5.5, xend=6.5, y=3.2, yend=2.8, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=6.5, xend=9,   y=3.2, yend=2.8, arrow=arrow(length=unit(0.3,"cm"))) +
  
  # Arrows (outputs to decision maker)
  annotate("segment", x=1.5, xend=3.5, y=1.5, yend=1.2, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=4,   xend=4.5, y=1.5, yend=1.2, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=6.5, xend=5.5, y=1.5, yend=1.2, arrow=arrow(length=unit(0.3,"cm"))) +
  annotate("segment", x=9,   xend=6.5, y=1.5, yend=1.2, arrow=arrow(length=unit(0.3,"cm"))) +
  
  # Title
  labs(title = "Hotel Revenue Intelligence System (HRIS) — Conceptual Framework") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, fontface = "bold")) +
  xlim(0, 10.5) + ylim(-0.2, 10)

print(diagram)