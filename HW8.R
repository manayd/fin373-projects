#a
electric = read.csv("~/Downloads/electric-company.csv")
summary(electric)
class(electric$grade)
electric$grade_factor <- factor(electric$grade)
electric$treatment <- ifelse(electric$treatment == "T", 1, 0)
#b
model_treatment <- lm(post.score ~ treatment, data = electric)
summary(model_treatment)
model_grade_treatment <- lm(post.score ~ grade_factor + treatment, data = electric)
summary(model_grade_treatment)
#c
model_grade_pre <- lm(post.score ~ grade_factor + pre.score, data = electric)
summary(model_grade_pre)
#d
# Define a function named fit_reg() that returns the coefficient on treatment
fit_reg <- function(data_all, grade_subset) {
  model <- lm(post.score ~ treatment + pre.score, data = data_all, subset = grade == grade_subset)
  coef(model)["treatment"]
}

# Initialize a vector to store coefficient on treatment for each grade
treatment_effects <- numeric(4)

# Use a for loop and call the fit_reg() function for each grade
for (grade in 1:4) {
  treatment_effects[grade] <- fit_reg(electric, grade)
  
  # Print out the coefficient on treatment for each grade
  print(paste("Grade", grade, "treatment effect:", treatment_effects[grade]))
}

# Comment on the result
cat("Treatment effects across grades:", treatment_effects, "\n")
#e
# Fit the fully interacted model
fully_interacted_model <- lm(post.score ~ pre.score * grade_factor * treatment, data = electric)
summary(fully_interacted_model)

# Construct grade-specific treatment effects
# For grade 2
grade_2_treatment_effect <- coef(fully_interacted_model)["pre.score:grade_factor2:treatment"] + 
  coef(fully_interacted_model)["pre.score:grade_factor2:treatment"]
print(paste("Grade 2 treatment effect:", grade_2_treatment_effect))

