#1
#a
justices = read.csv("~/Downloads/justices.csv")
library(dplyr)
median_ideal_by_year <- justices %>% 
  group_by(term) %>%
  summarize(median_ideal_point = median(idealpt, na.rm = TRUE))
median_ideal_by_year
library(ggplot2)
ggplot(median_ideal_by_year, aes(x = term, y = median_ideal_point)) +
  geom_line() +
  labs(title = "Median Ideal Point Over Time",
       x = "Year",
       y = "Median Ideal Point")
#b
merged_data <- merge(justices, median_ideal_by_year, by = "term")

justice_with_median <- merged_data %>%
  filter(idealpt == median_ideal_point) %>%
  group_by(term) %>%
  slice(which.min(abs(idealpt - median_ideal_point))) %>%
  ungroup()

justice_counts <- table(justice_with_median$justice)

most_common_justice <- names(justice_counts)[which.max(justice_counts)]

print(justice_with_median)
print(justice_counts)
print(paste("Justice with the most appearances:", most_common_justice))
#c
democratic_presidents <- unique(justices$pres[justices$pparty == "D"])
republican_presidents <- unique(justices$pres[justices$pparty == "R"])
democratic_ideology_shifts <- numeric(0)
republican_ideology_shifts <- numeric(0)

#d
for(president in democratic_presidents) {
  president_data <- justice_with_median %>% filter(pres == president)
  ideology_shift <- last(president_data$median_ideal_point) - first(president_data$median_ideal_point)
  democratic_ideology_shifts <- c(democratic_ideology_shifts, ideology_shift)
}

for(president in republican_presidents) {
  president_data <- justice_with_median %>% filter(pres == president)
  ideology_shift <- last(president_data$median_ideal_point) - first(president_data$median_ideal_point)
  republican_ideology_shifts <- c(republican_ideology_shifts, ideology_shift)
}

print("Democratic Ideology Shifts:")
print(democratic_ideology_shifts)

print("Republican Ideology Shifts:")
print(republican_ideology_shifts)

#e
mean(democratic_ideology_shifts)
sd(democratic_ideology_shifts)
mean(republican_ideology_shifts)
sd(republican_ideology_shifts)
print("Reagan had the largest conservative shift on the Court")
print("Kennedy had the largest liberal shift on the Court")

#f
plot <- ggplot(median_ideal_by_year, aes(x = term, y = median_ideal_point)) +
  geom_line(color = "black", linewidth = 2) +  # Black line for overall median ideal point
  labs(title = "Median Supreme Court Ideal Point Over Time",
       x = "Term",
       y = "Median Ideal Point")

plot <- plot +
  geom_line(data = justice_with_median, aes(x = term, y = idealpt, color = pparty), size = 1) +
  geom_text(data = justice_with_median %>% distinct(justice, .keep_all = TRUE),
            aes(x = term, y = idealpt, label = justice, color = pparty),
            vjust = -0.5, hjust = 0.5, size = 3) +
  scale_color_manual(values = c("blue", "red"), guide = FALSE)  # Blue for Democratic, Red for Republican

# Show the plot
print(plot)

#2
#a
mother_df = read.csv("~/Downloads/yu2017sample.csv")
length(unique(mother_df$PUBID))
summary(mother_df)
length(unique(mother_df$PUBID)) / 15

#b
children_counts <- table(mother_df$numChildren)

#c
mother_df$isMother <- ifelse(mother_df$numChildren > 0, 1, 0)
has_children_count <- table(mother_df$isMother)

#d
mother_df$logwage <- log(mother_df$wage)
wage_plot <- ggplot(mother_df, aes(x = factor(educ), y = wage)) +
  geom_boxplot() +
  labs(title = "Boxplot of Wage by Educational Level",
       x = "Educational Level",
       y = "Wage")
wage_plot
logwage_plot <- ggplot(mother_df, aes(x = factor(educ), y = logwage)) +
  geom_boxplot() +
  labs(title = "Boxplot of Wage by Educational Level",
       x = "Educational Level",
       y = "Wage")
logwage_plot

#e
logwage_plot <- ggplot(mother_df, aes(x = year, y = logwage, color = factor(isMother))) +
  geom_line(stat = "summary", fun = "mean", size = 1, linetype = "solid") +
  labs(title = "Mean Logwage Over Time for Mothers and Non-Mothers",
       x = "Year",
       y = "Mean Logwage",
       color = "Mother Status") +
  scale_color_manual(values = c("blue", "red"), labels = c("Non-Mothers", "Mothers")) +
  theme_minimal()
logwage_plot
#f
woman_fe <- as.factor(mother_df$PUBID)
year_fe <- as.factor(mother_df$year)

# Run the fixed effects regression using lm
fixed_effects_model <- lm(logwage ~ numChildren + fullTime + multipleLocations + unionized + industry, data = mother_df)
summary(fixed_effects_model)
#0.015702














