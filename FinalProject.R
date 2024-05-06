#final project
library(ggplot2)
library(plotly)

og_combined = read.csv("~/Downloads/combined.csv")
combined = read.csv("~/Downloads/combined.csv")
combined <- na.omit(combined)
combined$Density..P.Km2.= as.numeric(combined$Density..P.Km2.)
colnames(combined)
combined$Agricultural.Land.... <- as.numeric(gsub("%", "", combined$Agricultural.Land....))
combined$Land.Area.Km2.= as.numeric(gsub(",", "", combined$Land.Area.Km2.))
combined$Armed.Forces.size = as.numeric(gsub(",", "", combined$Armed.Forces.size))
combined$Co2.Emissions = as.numeric(gsub(",", "", combined$Co2.Emissions))
combined$CPI = as.numeric(combined$CPI)
combined$CPI.Change.... <- as.numeric(gsub("%", "", combined$CPI.Change....))
combined$Forested.Area.... <- as.numeric(gsub("%", "", combined$Forested.Area....))
combined$Gasoline.Price <- as.numeric(gsub("\\$", "", combined$Gasoline.Price))
combined$GDP <- as.numeric(gsub("[\\$,]", "", combined$GDP))
combined$Gross.primary.education.enrollment.... <- as.numeric(gsub("%", "", combined$Gross.primary.education.enrollment....))
combined$Gross.tertiary.education.enrollment.... <- as.numeric(gsub("%", "", combined$Gross.tertiary.education.enrollment....))
combined$Minimum.wage <- as.numeric(gsub("\\$", "", combined$Minimum.wage))
combined$Out.of.pocket.health.expenditure <- as.numeric(gsub("%", "", combined$Out.of.pocket.health.expenditure))
combined$Population = as.numeric(gsub(",", "", combined$Population))
combined$Population..Labor.force.participation.... <- as.numeric(gsub("%", "", combined$Population..Labor.force.participation....))
combined$Tax.revenue.... <- as.numeric(gsub("%", "", combined$Tax.revenue....))
combined$Total.tax.rate <- as.numeric(gsub("%", "", combined$Total.tax.rate))
combined$Unemployment.rate <- as.numeric(gsub("%", "", combined$Unemployment.rate))
combined$Urban_population = as.numeric(gsub(",", "", combined$Urban_population))
combined <- subset(combined, select = -c(Indicator.Name, Indicator.Code))
summary(combined)
combined$logGDP <- log(combined$GDP)
combined$logPopulation <- log(combined$Population)
combined$logX2018 <- log(combined$X2018)
combined$X2018perCapita <- combined$X2018 / combined$Population


par(mfrow = c(1, 1))
# Step 1: Data Preparation
relevant_columns <- c("X2018", "logX2018","logGDP", "GDP","Forested.Area....", "Freedom.to.make.life.choices", "Latitude", "Overall.rank","logPopulation","Population", "Life.expectancy", "Infant.mortality", "Tax.revenue....", "Co2.Emissions", "CPI")
relevant_data <- combined[, relevant_columns]

# Step 2: Data Exploration and Visualization
pairs(relevant_data)

# Boxplot to compare tourism across different categories (e.g., regions)
boxplot(X2018 ~ Population, data = combined, main = "Tourism versus Population", xlab = "Population", ylab= "Tourism")
boxplot(X2018 ~ Latitude, data = combined, main = "Tourism versus Latitude", xlab = "Latitude", ylab= "Tourism")
boxplot(X2018 ~ Overall.rank, data = combined, main = "Tourism versus Overall Rank", xlab = "Overall Rank", ylab= "Tourism")
boxplot(X2018 ~ GDP, data = combined, main = "Tourism versus GDP", xlab = "GDP", ylab= "Tourism")
boxplot(X2018 ~ GDP.per.capita, data = combined)

# Step 3: Correlation Analysis
correlation_matrix <- cor(relevant_data)
print(correlation_matrix)

# Step 4: Regression Analysis
lm_model <- lm(X2018 ~ . - logX2018, data = relevant_data)
summary(lm_model)

# Step 5: Model Evaluation
rsquared <- summary(lm_model)$r.squared
print(rsquared)

# Step 6: Visualization of Results
plot(relevant_data$Overall.rank, relevant_data$X2018, xlab = "GDP", ylab = "Tourism (X2018)")
abline(lm_model, col = "red")

# Residual plot
plot(lm_model, which = 1, main = "Residuals vs Fitted Values")

# Barplot to compare tourism across different countries or regions
combined_ordered <- combined[order(combined$X2018, decreasing = TRUE), ]
ggplot(combined_ordered, aes(x = reorder(Country, X2018), y = X2018)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Tourism by Country", x = "Country", y = "X2018") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))  # Rotate x-axis labels for better readability

# Histograms to visualize distributions of variables
par(mfrow = c(2, 2))
hist(relevant_data$GDP, main = "Distribution of GDP", xlab = "GDP")
hist(relevant_data$Population, main = "Distribution of Population", xlab = "Population")
hist(relevant_data$Life.expectancy, main = "Distribution of Life Expectancy", xlab = "Life Expectancy")
hist(relevant_data$Infant.mortality, main = "Distribution of Infant Mortality", xlab = "Infant Mortality")
hist(relevant_data$logGDP, main = "Distribution of Log of GDP", xlab = "log GDP")
hist(relevant_data$logPopulation, main = "Distribution of Log of Population", xlab = "log Population")
hist(relevant_data$X2018perCapital, main = "Distribution of Tourism Per Capita", xlab = "Tourism Per Capita")

# Boxplot or violin plot to compare tourism across different categories
boxplot(X2018 ~ GDP, data = combined, main = "Tourism by Region")

# Interactive visualizations using plotly
plot_ly(data = relevant_data, x = ~GDP, y = ~X2018, type = "scatter", mode = "markers") %>%
  add_markers() %>%
  add_lines(y = ~fitted(lm_model), line = list(color = "red"))

plot_ly(data = relevant_data, x = ~fitted(lm_model), y = ~residuals(lm_model), type = "scatter", mode = "markers")

#highest and lowest plot ----------------
# Sort the dataframe based on tourism data (X2018 column)
sorted_combined <- combined[order(combined$X2018, decreasing = TRUE), ]

# Top 10 highest tourism countries
top_10_highest <- head(sorted_combined, 10)

# Lowest 10 tourism countries
lowest_10 <- tail(sorted_combined, 10)

# Create separate bar plots for the top and lowest countries
plot_top <- ggplot(top_10_highest, aes(x = reorder(Country.Name, -X2018), y = X2018)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Country", y = "Tourism", title = "Top 10 Highest Tourism Countries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, max(sorted_combined$X2018))  # Set the y-axis limits to the maximum value of X2018

plot_lowest <- ggplot(lowest_10, aes(x = reorder(Country.Name, -X2018), y = X2018)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Country", y = "Tourism", title = "Lowest 10 Tourism Countries") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(0, max(sorted_combined$X2018 / 5))  # Set the y-axis limits to the maximum value of X2018

# Show the two plots separately
plot_top
plot_lowest


#latitude plot ----------------
summary(combined)

ranges <- seq(-40, 70, by = 8)

# Cut latitude data into groups
relevant_data$Group <- cut(relevant_data$Latitude, breaks = ranges, labels = paste(ranges[-length(ranges)], ranges[-1], sep = "-"))

# Calculate mean tourism for each latitude group
mean_tourism <- aggregate(X2018 ~ Group, data = relevant_data, FUN = mean)

# Create bar plot to compare mean tourism
ggplot(mean_tourism, aes(x = Group, y = X2018)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Latitude Group", y = "Mean Tourism", title = "Mean Tourism Across Latitude Groups") +
  theme_minimal()


#DiD plot ----------------
# Calculate the difference between tourism data for 2018 and 2016
combined$Difference <- (combined$X2018 - combined$X2016) / combined$X2016

# Sort the dataframe based on the difference
sorted_data <- combined[order(combined$Difference), ]

# Top 10 countries with the highest difference
top_10_highest_diff <- tail(sorted_data, 10)

# Lowest 10 countries with the lowest difference
lowest_10_lowest_diff <- head(sorted_data, 10)

# Create plots for the highest and lowest countries
#par(mfrow = c(2, 1))  # Arrange plots in 2 rows
barplot(top_10_highest_diff$Difference, main = "Top 10 Countries with Highest Difference (2018 - 2016)", xlab = "Country", ylab = "Difference", names.arg = top_10_highest_diff$Country, col = "skyblue")
barplot(lowest_10_lowest_diff$Difference, main = "Lowest 10 Countries with Lowest Difference (2018 - 2016)", xlab = "Country", ylab = "Difference", names.arg = lowest_10_lowest_diff$Country, col = "lightgreen")

# Calculate the difference between the highest and lowest countries
highest_difference <- max(top_10_highest_diff$Difference)
lowest_difference <- min(lowest_10_lowest_diff$Difference)
overall_difference <- highest_difference - lowest_difference

# Extract the names of the countries with the highest and lowest differences
highest_country <- top_10_highest_diff$Country[which.max(top_10_highest_diff$Difference)]
lowest_country <- lowest_10_lowest_diff$Country[which.min(lowest_10_lowest_diff$Difference)]

# Print out the names of the countries with the highest and lowest differences
cat("Country with the highest difference:", highest_country, "\n")
cat("Country with the lowest difference:", lowest_country, "\n")
cat("Difference between the highest and lowest countries:", overall_difference, "\n")


#clustering ------------
data_for_clustering <- relevant_data[, c("logGDP", "logPopulation", "X2018")]

# Calculate WCSS for different values of k
wcss <- numeric(length = 10)  # Initialize a vector to store WCSS values

for (i in 1:10) {
  kmeans_result <- kmeans(data_for_clustering, centers = i)
  wcss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow plot
plot(1:10, wcss, type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares (WCSS)", main = "Elbow Plot")

# Set the number of clusters (k)
k <- 3

# Perform k-means clustering
kmeans_result <- kmeans(data_for_clustering, centers = k)

# Plot 1: GDP vs Population with cluster assignments
plot(data_for_clustering[, c("logGDP", "logPopulation")], col = kmeans_result$cluster, main = "Clusters: GDP vs Population", xlab = "GDP", ylab = "Population")
points(kmeans_result$centers[, c("logGDP", "logPopulation")], col = 1:k, pch = 8, cex = 2)

# Plot 2: GDP vs X2018 with cluster assignments
plot(data_for_clustering[, c("logGDP", "X2018")], col = kmeans_result$cluster, main = "Clusters: GDP vs Tourism", xlab = "GDP", ylab = "Tourism")
points(kmeans_result$centers[, c("logGDP", "X2018")], col = 1:k, pch = 8, cex = 2)

# Plot 3: Population vs X2018 with cluster assignments
plot(data_for_clustering[, c("logPopulation", "X2018")], col = kmeans_result$cluster, main = "Clusters: Population vs Tourism", xlab = "Population", ylab = "Tourism")
points(kmeans_result$centers[, c("logPopulation", "X2018")], col = 1:k, pch = 8, cex = 2)

# Print the cluster assignments for each data point
print(kmeans_result$cluster)

# Combine the cluster assignments with the original dataframe
combined_with_clusters <- cbind(combined, Cluster = kmeans_result$cluster)

# Print the countries in each cluster
for (i in unique(kmeans_result$cluster)) {
  cat("Cluster", i, ": ", paste(combined_with_clusters$Country[combined_with_clusters$Cluster == i], collapse = ", "), "\n")
}

# Assuming 'relevant_data' contains the data for clustering
# Remove rows with missing values
relevant_data_clean <- na.omit(relevant_data)

# Perform PCA on the cleaned data
pca_result <- prcomp(relevant_data_clean[, -1], scale. = TRUE)  # Exclude the first column (assumed to be the country names)

# Extract the principal components
pca_data <- as.data.frame(pca_result$x)

# Perform hierarchical clustering on the principal components
hc <- hclust(dist(pca_data))

# Plot the dendrogram
plot(hc, main = "Dendrogram of Hierarchical Clustering with PCA")

# Cut the dendrogram to obtain clusters
# For example, cutting the dendrogram into 3 clusters
cluster_cut <- cutree(hc, k = 3)

# Combine the cluster assignments with the original dataframe
combined_with_clusters <- cbind(relevant_data_clean, Cluster = cluster_cut)

# Print the countries in each cluster
for (i in unique(cluster_cut)) {
  cat("Cluster", i, ": ", paste(combined_with_clusters$Country[combined_with_clusters$Cluster == i], collapse = ", "), "\n")
}

