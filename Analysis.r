install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("multcomp")
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(multcomp)
file_path <- file.choose()
data1 <- read_excel(file_path)
# Data Cleaning and Preparation
data1 <- data1 %>%
  rename(Group = `System ID`)

# Convert Group to factor for clearer results
data1$Group <- factor(data1$Group, labels = c("High Anchor", "Low Anchor", "Control"))

# Define numeric questions for analysis
numeric_questions <- names(data1)[sapply(data1, is.numeric)]
numeric_questions <- setdiff(numeric_questions, "Group")

# General function for descriptive statistics and anchoring effect analysis
analyze_question <- function(question) {
  data_filtered <- data1 %>% filter(!is.na(.data[[question]]))

  # Descriptive statistics
  desc_stats <- data_filtered %>%
    group_by(Group) %>%
    summarise(Mean = mean(.data[[question]], na.rm = TRUE),
              Median = median(.data[[question]], na.rm = TRUE),
              SD = sd(.data[[question]], na.rm = TRUE))

  print(paste("Descriptive statistics for", question))
  print(desc_stats)

  # Visualize descriptive statistics
  plot <- ggplot(data_filtered, aes(x = Group, y = .data[[question]], fill = Group)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste(question, "by Anchor Group"),
         x = "Anchor Group",
         y = question) +
    theme_minimal()
  print(plot)

  # T-tests and Effect size calculation
  high_vs_control <- t.test(.data[[question]] ~ Group, data = data_filtered %>% filter(Group %in% c("High Anchor", "Control")))
  low_vs_control <- t.test(.data[[question]] ~ Group, data = data_filtered %>% filter(Group %in% c("Low Anchor", "Control")))

  print(high_vs_control)
  print(low_vs_control)

  effect_high <- cohen.d(.data[[question]] ~ Group, data = data_filtered %>% filter(Group %in% c("High Anchor", "Control")))
  effect_low <- cohen.d(.data[[question]] ~ Group, data = data_filtered %>% filter(Group %in% c("Low Anchor", "Control")))

  print(effect_high)
  print(effect_low)

  cat("Anchoring Effect (High Anchor):", ifelse(high_vs_control$p.value < 0.05, "Significant", "Not Significant"), "\n")
  cat("Anchoring Effect (Low Anchor):", ifelse(low_vs_control$p.value < 0.05, "Significant", "Not Significant"), "\n")
}

# Apply analysis to all numeric questions
for(question in numeric_questions) {
  analyze_question(question)
}

