load("~/Downloads/newspapers.RData")
# Load necessary libraries
library(ggplot2)

# Load the papers dataset
# Assuming you have the dataset in a file named "papers.csv"

# Check the structure of the papers dataset
str(papers)

# Plot the distribution of ideological slant (nslant)
ggplot(papers, aes(x = nslant)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = median(nslant)), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Ideological Slant in Newspapers",
       x = "Ideological Slant (nslant)",
       y = "Frequency") +
  theme_minimal()

# Identify the newspaper with the largest left-wing slant
left_most_slant <- papers[which.min(papers$nslant), ]

# Identify the newspaper with the largest right-wing slant
right_most_slant <- papers[which.max(papers$nslant), ]

# Print the results
cat("Newspaper with the largest left-wing slant:", left_most_slant$paper, "\n")
cat("Newspaper with the largest right-wing slant:", right_most_slant$paper, "\n")

#b

# Load necessary libraries
library(wordcloud)

# Assuming you already have the dtm object

# Extract terms and their frequencies from dtm
terms <- dtm$dimnames$Terms
freq <- colSums(as.matrix())

# Create a data frame with terms and their frequencies
dtm_df <- data.frame(terms = terms, freq = dtm$ncol)

# Sort the data frame by frequency in descending order
dtm_df <- dtm_df[order(-dtm_df$freq), ]

# Create a word cloud
wordcloud(words = dtm_df$terms, freq = dtm_df$freq, max.words = 20)

# Subset the data to the tenth of newspapers with the leftmost (lowest) political slant
left_most_newspapers <- papers[order(papers$nslant)][1:round(nrow(papers)/10), ]

# Subset the data to the tenth of newspapers with the rightmost (highest) political slant
right_most_newspapers <- papers[order(papers$nslant, decreasing = TRUE)][1:round(nrow(papers)/10), ]

# Convert dtm matrix into a data frame
dtm_df <- as.data.frame(dtm)

# Subset dtm_df based on left-most and right-most newspapers
left_most_dtm <- dtm_df[rownames(dtm_df) %in% left_most_newspapers$newsid, ]
right_most_dtm <- dtm_df[rownames(dtm_df) %in% right_most_newspapers$newsid, ]

# Create word clouds for left-most and right-most newspapers
wordcloud(words = rownames(left_most_dtm), freq = colSums(left_most_dtm), max.words = 20)
wordcloud(words = rownames(right_most_dtm), freq = colSums(right_most_dtm), max.words = 20)

#c

# Compute average slant by state for House
house_avg_slant <- aggregate(cslant ~ state, data = subset(cong, chamber == "H"), FUN = mean)

# Compute average slant by state for Senate
senate_avg_slant <- aggregate(cslant ~ state, data = subset(cong, chamber == "S"), FUN = mean)

# Compute average newspaper slant by state
newspaper_avg_slant <- aggregate(nslant ~ state, data = papers, FUN = mean)

print(dim(house_avg_slant))
print(dim(newspaper_avg_slant))

# Check the column names of house_avg_slant and newspaper_avg_slant
print(colnames(house_avg_slant))
print(colnames(newspaper_avg_slant))
# Plot for House
ggplot() +
  geom_point(data = house_avg_slant, aes(x = cslant, y = newspaper_avg_slant$nslant[2:51]), color = "green") +
  geom_smooth(data = house_avg_slant, aes(x = cslant, y = newspaper_avg_slant$nslant[2:51]), method = "lm", color = "red") +
  labs(title = "Comparison of House Congressional Slant and Newspaper Slant by State",
       x = "House Congressional Slant",
       y = "Newspaper Slant") +
  theme_minimal()

# Plot for Senate
ggplot() +
  geom_point(data = senate_avg_slant, aes(x = cslant, y = newspaper_avg_slant$nslant[2:51]), color = "green") +
  geom_smooth(data = senate_avg_slant, aes(x = cslant, y = newspaper_avg_slant$nslant[2:51]), method = "lm", color = "red") +
  labs(title = "Comparison of Senate Congressional Slant and Newspaper Slant by State",
       x = "Senate Congressional Slant",
       y = "Newspaper Slant") +
  theme_minimal()

#d

# Subset cong and papers datasets to include only data from Texas
cong_tx <- subset(cong, state == "TX")
papers_tx <- subset(papers, state == "TX")

# Merge cong_tx and papers_tx datasets by district and state
merged_tx <- merge(cong_tx, papers_tx, by = c("district", "state"))

# Compute average slant by district for House
house_avg_slant_tx <- aggregate(cslant ~ district, data = subset(merged_tx, chamber == "H"), FUN = mean)

# Compute average newspaper slant by district
newspaper_avg_slant_tx <- aggregate(nslant ~ district, data = merged_tx, FUN = mean)

# Plot at district level for House
ggplot() +
  geom_point(data = house_avg_slant_tx, aes(x = cslant, y = newspaper_avg_slant_tx$nslant), color = "green") +
  geom_smooth(data = house_avg_slant_tx, aes(x = cslant, y = newspaper_avg_slant_tx$nslant), method = "lm", color = "red") +
  labs(title = "Comparison of House Congressional Slant and Newspaper Slant by District in Texas",
       x = "House Congressional Slant",
       y = "Newspaper Slant") +
  theme_minimal()

#e

# Load necessary libraries
library(tm)

# Assuming you have the dtm object

# Convert dtm into a TermDocumentMatrix
dtm_tdm <- TermDocumentMatrix(dtm)

# Compute TF-IDF
tfidf <- weightTfIdf(dtm_tdm)

# Convert TF-IDF matrix into a data frame
tfidf_df <- as.data.frame(as.matrix(tfidf))

# Subset TF-IDF transformed matrix to contain data from the "Home News Tribune" of East Brunswick, NJ
home_news_tribune <- tfidf_df[rownames(tfidf_df) == "Home News Tribune", ]

# Print the terms with the largest TF-IDF in decreasing order
top_terms <- sort(home_news_tribune, decreasing = TRUE)[1:10]
print(top_terms)

