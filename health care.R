# Load necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2) # For melting the correlation matrix

# Load the dataset
data <- read.csv('healthcare_dataset.csv')

# 1. Summary Statistics for numerical columns
summary_stats <- summary(select(data, Age, Billing.Amount, Room.Number))
print(summary_stats)

# 2. Frequency counts for categorical columns
medical_condition_freq <- table(data$Medical.Condition)
gender_freq <- table(data$Gender)

print("Medical Condition Frequency:")
print(medical_condition_freq)

print("Gender Frequency:")
print(gender_freq)

# 3. Correlation matrix for numerical columns
correlation_matrix <- cor(select(data, Age, Billing.Amount, Room.Number), use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# 4. Checking for missing values
missing_values <- colSums(is.na(data))
print("Missing Values:")
print(missing_values)

# ---- Plots and Graphs ----

# 1. Age Distribution Histogram
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# 2. Billing Amount Distribution Histogram
ggplot(data, aes(x = Billing.Amount)) +
  geom_histogram(binwidth = 5000, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Billing Amount Distribution", x = "Billing Amount", y = "Frequency") +
  theme_minimal()

# 3. Medical Condition Frequency Bar Plot
ggplot(data, aes(x = reorder(Medical.Condition, -table(Medical.Condition)[Medical.Condition]))) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Frequency of Medical Conditions", x = "Medical Condition", y = "Count") +
  theme_minimal() +
  coord_flip()

# 4. Gender Distribution Pie Chart
gender_dist <- data %>%
  group_by(Gender) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(gender_dist, aes(x = "", y = percentage, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Gender Distribution") +
  theme_void() +
  theme(legend.position = "right")

# 5. Correlation Heatmap
# Melt the correlation matrix for ggplot
melted_corr <- melt(correlation_matrix)

ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

