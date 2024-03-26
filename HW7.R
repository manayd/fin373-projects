#1
#a
survey_data = read.csv("~/Downloads/finlit15and18.csv")
# Load required library for bootstrap analysis
library(boot)

# a. First, how many observations are in the data?
num_observations <- nrow(survey_data)
cat("Number of observations in the data:", num_observations, "\n")

# Second, describe the survey respondents across some of the variables.

# Gender and age breakdown
gender_breakdown <- table(survey_data$A3)
age_breakdown <- summary(survey_data$A3A)

cat("Gender breakdown:\n", gender_breakdown, "\n")
cat("Age breakdown:\n", age_breakdown, "\n")

# Distribution of respondents across household income
income_breakdown <- table(survey_data$A8)
cat("Household income breakdown:\n", income_breakdown, "\n")

#b
# b. Compute the average literacy difference between females and males.
# Group data by gender
gender_grouped <- split(survey_data$literacy, survey_data$A3)
# Calculate mean literacy for each gender
mean_literacy_female <- mean(gender_grouped$"1")
mean_literacy_male <- mean(gender_grouped$"2")
# Compute observed difference
observed_difference <- mean_literacy_female - mean_literacy_male

# Define function to compute difference in means
diff_mean <- function(data, indices) {
  female_mean <- mean(data[indices[1]][data$A3 == "Female"])
  male_mean <- mean(data[indices[2]][data$A3 == "Male"])
  return(female_mean - male_mean)
}

# Bootstrap to characterize sampling distribution of difference
set.seed(123) # for reproducibility
boot_result <- boot(survey_data, diff_mean, R = 1000)
# Get 95% confidence interval
confidence_interval <- quantile(boot_result$t, c(0.025, 0.975))

# Output results
cat("Average literacy difference between females and males:", observed_difference, "\n")
cat("95% Confidence Interval:", confidence_interval, "\n")

#c
# Fit simple linear regression model
fit <- lm(literacy ~ factor(A3), data = survey_data)
# Report coefficient and standard error on the gender variable
summary(fit)

# Bootstrap of coefficient to characterize sampling distribution
coef_boot <- function(data, indices) {
  fit <- lm(literacy ~ factor(A3), data = data[indices, ])
  return(coef(fit)[2])  # Coefficient for gender variable
}

# Bootstrap to characterize sampling distribution
boot_result_coef <- boot(survey_data, coef_boot, R = 1000)
# Get standard deviation of sampling distribution
sampling_std <- sd(boot_result_coef$t)
# Compare to standard error from regression output
regression_se <- summary(fit)$coefficients[2, "Std. Error"]

cat("Standard deviation of sampling distribution:", sampling_std, "\n")
cat("Standard error from regression output:", regression_se, "\n")

#d
# Fit regression models with different numbers of variables
# Start with a small model and move to a large model

# Small model with a couple of variables
small_model <- lm(Y ~ literacy + A8, data = survey_data)
summary(small_model)

# Large model with all 12 variables
large_model <- lm(Y ~ literacy + A5_2015 + A3A + J2 + A3 + A8 + E20 + F2_2 + F2_3 + F2_4 + F2_5 + F2_6, data = survey_data)
summary(large_model)

# Compare models and assess variation in perceived economic condition
# Check how the literacy effect changes across models

# Check if literacy effect changes across models
literacy_effect_small <- coef(small_model)["literacy"]
literacy_effect_large <- coef(large_model)["literacy"]

cat("Literacy effect in small model:", literacy_effect_small, "\n")
cat("Literacy effect in large model:", literacy_effect_large, "\n")

# Assess how well the models describe the variation in perceived economic condition
# Compare R-squared values across models
rsquared_small <- summary(small_model)$r.squared
rsquared_large <- summary(large_model)$r.squared

cat("R-squared value for small model:", rsquared_small, "\n")
cat("R-squared value for large model:", rsquared_large, "\n")

#2
#a
transfer_data = read.csv("~/Downloads/transfer.csv")
# Define population cutoffs
# Define population cutoffs
cutoffs <- c(10188, 13584, 16980)

# Calculate the difference from the closest population cutoff for each municipality
transfer_data$closest_cutoff <- sapply(transfer_data$pop82, function(x) min(abs(x - cutoffs)))

# Standardize the measure
transfer_data$normalized_percent_score <- (transfer_data$closest_cutoff / cutoffs) * 100

#b
# Subset data
subset_data <- transfer_data[transfer_data$normalized_percent_score <= 3, ]

model_poverty <- lm(poverty91 - poverty80 ~ 1, data = subset_data)
model_educ <- lm(educ91 - educ80 ~ 1, data = subset_data)

# Summarize results
summary(model_poverty)
summary(model_educ)

#c
# Plot data points and fitted regression lines for literacy rate
plot(subset_data$pop82, subset_data$poverty91 - subset_data$poverty80, xlab = "Population", ylab = "Change in Poverty Rate (1991 - 1980)", main = "Change in Literacy Rate vs. Population")
abline(model_poverty, col = "blue")
legend("topleft", legend = c("Fitted Regression Line"), col = c("blue"), lty = 1:2)

plot(subset_data$pop82, subset_data$educ91 - subset_data$educ80, xlab = "Population", ylab = "Change in Literacy Rate (1991 - 1980)", main = "Change in Literacy Rate vs. Population")
abline(model_educ, col = "blue")
legend("topleft", legend = c("Fitted Regression Line"), col = c("blue"), lty = 1:2)

#d
# Create binary variable indicating whether population is above or below threshold
subset_data$above_threshold <- ifelse(subset_data$pop82 > subset_data$closest_cutoff, 1, 0)

# Compute difference in means
mean_diff_educ <- tapply(subset_data$educ91, subset_data$above_threshold, mean)
mean_diff_literate <- tapply(subset_data$literate91, subset_data$above_threshold, mean)
mean_diff_poverty <- tapply(subset_data$poverty91, subset_data$above_threshold, mean)
