---
title: "Anchoring Effect Survey Analysis"
author: "Florian Kupczik"
date: "`r Sys.Date()`"
output:
  html_document: default
  pdf_document: default
---

```{r}
knitr::opts_chunk$set(echo = TRUE)
```

## Vorbereitung und Pakete

```{r}
install.packages(c("readxl", "dplyr", "ggplot2", "effsize", "effectsize", "knitr", "tidyr"), repos="https://cran.rstudio.com/")
library(readxl)
library(dplyr)
library(ggplot2)
library(effsize)
library(knitr)
library(tidyr)


High <- read_excel("C:/Users/dange/OneDrive/Uni/Sem8; Fin/BA/Survey results/Clean/High Anchor.xlsx") %>% mutate(group = "High")
Low <- read_excel("C:/Users/dange/OneDrive/Uni/Sem8; Fin/BA/Survey results/Clean/Low Anchor.xlsx") %>% mutate(group = "Low")
Control <- read_excel("C:/Users/dange/OneDrive/Uni/Sem8; Fin/BA/Survey results/Clean/Control.xlsx") %>% mutate(group = "Control")

Data <- bind_rows(High, Low, Control)
```
## Sorting and cleaning Data, assigning Values and Collumns to the anchors
```{r}
colnames(Data) <- make.names(colnames(Data))

Data$age <- Data$How.old.are.you...Numeric.answer.please.
Data$gender <- Data$Which.gender.do.you.identify.with.
Data$education <- Data$What.is.your.current.education.level.
Data$occupation <- Data$What.are.you.currently.doing.full.time.
Data$field <- Data$What.is.your.current.field.of.study.or.work.
Data$estimation_confidence <- Data$How.confident.are.you.in.your.numerical.estimations...Like.guessing.the.price.of.an.item.
Data$good_with_numbers <- Data$Do.you.consider.yourself.good.with.numbers.
Data$instruction_google <- Data$What.is.the.instruction.regarding.external.help..e.g...Google..

clean_numeric <- function(x) {
  x <- gsub(",", ".", x)           # Replace comma with dot (if decimals are written with commas)
  x <- gsub("[^0-9.]", "", x)      # Remove everything except numbers and dots
  x[x == ""] <- NA                 # Convert empty strings to NA
  return(as.numeric(x))
}

Data$anchor_gandhi <- ifelse(
  Data$group == "High", Data$Was.Mahatma.Gandhi.older.or.younger.than.90.years.when.he.died.,
  ifelse(Data$group == "Low", Data$Was.Mahatma.Ghandi.older.or.younger.than.35.years.when.he.died., NA)
)

Data$gandhi_age <- Data$How.old.was.he.when.he.died...Numerical.answer.please.
Data$gandhi_age[is.na(Data$gandhi_age)] <- Data$How.old.was.Mahatma.Gandhi.when.he.died...Numerical.answer.please.[is.na(Data$gandhi_age)]
Data$gandhi_age <- as.numeric(Data$gandhi_age)

Data$anchor_africa <- ifelse(
  Data$group == "High", Data$Is.the.percentage.of.African.countries.in.the.UN.higher.or.lower.than.65..,
  ifelse(Data$group == "Low", Data$Is.the.percentage.of.African.countries.in.the.UN.higher.or.lower.than.10.., NA)
)

Data$guess_africa <- Data$What.is.your.best.estimate.of.the.percentage.of.African.Countries.in.the.UN...Numerical.answer.please.
Data$guess_africa <- as.numeric(Data$guess_africa) * ifelse(Data$group == "High", 1, 100)

Data$anchor_groceries <- ifelse(
  Data$group == "High", Data$Is.the.price.of.a.weekly.grocery.basket.for.one.person.more.or.less.than.115..,
  ifelse(Data$group == "Low", Data$Is.the.price.of.a.weekly.grocery.basket.for.one.person.more.or.less.than.55..,
         NA)
)

Data$guess_groceries <- Data$What.is.the.average.price.of.a.weekly.grocery.basket.for.one.person...Numerical.value.
Data$guess_groceries <- as.numeric(Data$guess_groceries)

Data$anchor_income <- ifelse(
  Data$group == "High", Data$Is.the.average.monthly.net.income.in.Germany..income.after.taxes..higher.or.lower.than.4000..,
  ifelse(Data$group == "Low", Data$Is.the.average.monthly.net.income.in.Germany.net..income.after.taxes..more.or.less.than.1000..,
         NA)
)

Data$guess_income <- Data$What.is.the.average.monthly.net.income..income.after.taxes..in.Germany.
Data$guess_income <- as.numeric(Data$guess_income)

Data$anchor_apples <- ifelse(
  Data$group == "High", Data$Does.a.kilogram.of.apples.in.a.supermarket.cost.more.or.less.than.4.50..,
  ifelse(Data$group == "Low", Data$Does.a.kilogram.of.apples.in.a.supermarket.cost.more.or.less.than.1.50..,
         NA)
)

Data$guess_apples <- Data$How.much.does.a.kilogram.of.apples.in.a.supermarket.cost...Answer.please.in.the.same.format.as.the.pricing.above.
Data$guess_apples[is.na(Data$guess_apples)] <- Data$How.much.does.a.kilogram.of.apples.in.a.supermarket.cost...Numerical.answer.please.[is.na(Data$guess_apples)]

Data$anchor_inflation <- ifelse(
  Data$group == "High", Data$Is.the.average.monthly.inflation.rate.in.Germany.higher.or.lower.than.8..,
  ifelse(Data$group == "Low", Data$Is.the.average.monthly.inflation.rate.in.Germany.higher.or.lower.than.1..,
         NA)
)
Data$guess_inflation <- gsub(",", ".", Data$What.is.your.estimate.of.the.monthly.inflation.rate.in.Germany...Numerical.answer.please..use.a.dot.....for.decimals.if.needed.)
Data$guess_inflation <- gsub("[^0-9.]", "", Data$guess_inflation)
Data$guess_inflation <- as.numeric(Data$guess_inflation)
Data$guess_inflation[Data$guess_inflation < 0 | Data$guess_inflation >= 1] <- NA

Data$anchor_saving <- ifelse(
  Data$group == "High", Data$Ideally..would.you.consider.saving.more.or.less.than.30..of.your.monthly.income.,
  ifelse(Data$group == "Low", Data$Ideally..would.you.consider.saving.more.or.less.than.5..of.your.monthly.income.,
         NA)
)
Data$guess_saving <- gsub(",", ".", Data$What.percentage.of.your.monthly.income.would.you.ideally.save...Numerical.answer.please..no.decimals.)
Data$guess_saving <- gsub("[^0-9.]", "", Data$guess_saving)
Data$guess_saving <- as.numeric(Data$guess_saving)
Data$guess_saving[Data$guess_saving < 0 | Data$guess_saving >= 1] <- NA

Data$lottery_choice <- Data$Imagine.winning.a.small.lottery..You.are.offered..60.today.or..70.in.6.months..Which.do.you.choose.

risk_col_1 <- "You.are.offered.two.options....a.guaranteed.payout.of..40..or.a.50..chance.to.win..60.and.a.50..chance.to.win..0..Which.option.would.you.prefer."
risk_col_2 <- "You.are.offered.two.options...a.guaranteed.payout.of..40..or.a.50..chance.to.win..60.and.a.50..chance.to.win..0..Which.option.would.you.prefer."
Data$risk_choice <- NA
if (risk_col_1 %in% names(Data)) {
  Data$risk_choice <- Data[[risk_col_1]]
}
if (risk_col_2 %in% names(Data)) {
  Data$risk_choice[is.na(Data$risk_choice)] <- Data[[risk_col_2]][is.na(Data$risk_choice)]
}
Data$risk_choice <- trimws(Data$risk_choice)
Data$demand_awareness <- ifelse(
  Data$group %in% c("High", "Low", "Control"),
  Data$Did.you.feel.that.the.survey.was.hinting.at.what.kind.of.answers.were.expected.from.you.,
  NA
)
Data$demand_reason <- ifelse(
  Data$group %in% c("High", "Low", "Control"),
  Data$If.yes..what.do.you.think.was.the.aim.of.this.survey.,
  NA
)
Data <- Data %>%
  filter(
    is.na(gandhi_age) | (gandhi_age >= 30 & gandhi_age <= 120),
    is.na(guess_africa) | (guess_africa >= 0 & guess_africa <= 100),
    is.na(guess_groceries) | (guess_groceries >= 5 & guess_groceries <= 200),
    is.na(guess_income) | (guess_income >= 500 & guess_income <= 10000),
    is.na(guess_apples) | (guess_apples >= 0.5 & guess_apples <= 10),
    is.na(guess_inflation) | (guess_inflation >= 0 & guess_inflation <= 0.2),
    is.na(guess_saving) | (guess_saving >= 0 & guess_saving <= 1)
  )
```
## Age & Estimation Confidence
```{r}
n_total <- nrow(Data)
numeric_summary <- data.frame(
  Category = character(),
  Result = character(),
  Mean = numeric(),
  Median = numeric(),
  Min = numeric(),
  Max = numeric(),
  N = numeric(),
  Percent = numeric(),
  stringsAsFactors = FALSE
)
age_vals <- na.omit(Data$age)
numeric_summary <- rbind(numeric_summary, data.frame(
  Category = "Age",
  Result = paste0("Mean: ", round(mean(age_vals), 1)),
  Mean = round(mean(age_vals), 1),
  Median = median(age_vals),
  Min = min(age_vals),
  Max = max(age_vals),
  N = length(age_vals),
  Percent = round(length(age_vals) / n_total * 100, 1)
))
conf_vals <- na.omit(Data$estimation_confidence)
numeric_summary <- rbind(numeric_summary, data.frame(
  Category = "Estimation Confidence",
  Result = paste0("Mean: ", round(mean(conf_vals), 2)),
  Mean = round(mean(conf_vals), 2),
  Median = median(conf_vals),
  Min = min(conf_vals),
  Max = max(conf_vals),
  N = length(conf_vals),
  Percent = round(length(conf_vals) / n_total * 100, 1)
))
kable(numeric_summary, caption = "Summary Statistics for Numeric Demographic Variables")
```
## Categorical Variables
```{r}
n_total2 <- nrow(Data)
gender_summary <- Data %>% 
  filter(!is.na(gender)) %>% 
  count(gender, sort = TRUE) %>%
  slice(1) %>%
  mutate(
    Category = "Most Common Gender",
    N = n,
    Percent = round(n / n_total2 * 100, 1)
  ) %>%
  select(Category, gender, N, Percent) %>%
  rename(Result = gender)
Data$education_grouped <- dplyr::case_when(
  Data$education %in% c("No degree") ~ "No degree",
  Data$education %in% c("Abitur", "Abitur + Ausbildung", "High school diploma", 
                        "Middle Qualification", "Realschule") ~ "High school",
  Data$education %in% c("Bachelor's", "Diploma", "University", 
                        "Get a pre-diploma and currently finishing my diploma") ~ "Bachelor’s",
  Data$education %in% c("Master's", "Fachwirt") ~ "Master’s",
  Data$education %in% c("PHd") ~ "PhD",
  TRUE ~ "Other"
)
education_summary <- Data %>% 
  filter(!is.na(education_grouped)) %>% 
  count(education_grouped, sort = TRUE) %>%
  slice(1) %>%
  mutate(
    Category = "Most Common Education",
    N = n,
    Percent = round(n / n_total2 * 100, 1)
  ) %>%
  select(Category, education_grouped, N, Percent) %>%
  rename(Result = education_grouped)
Data$occupation_grouped <- dplyr::case_when(
  Data$occupation == "Studying (School)" ~ "Student – School",
  Data$occupation == "Studying (University)" ~ "Student – University",
  Data$occupation %in% c("Work", "working part time") ~ "Employed",
  Data$occupation %in% c("Jobless", "Unemployed", "Looking for a job", 
                         "Unemployed, will start working in June", "Arbeitslos") ~ "Unemployed / Looking",
  TRUE ~ "Other / Not specified"
)
occupation_summary <- Data %>% 
  filter(!is.na(occupation_grouped)) %>% 
  count(occupation_grouped, sort = TRUE) %>%
  slice(1) %>%
  mutate(
    Category = "Most Common Occupation",
    N = n,
    Percent = round(n / n_total2 * 100, 1)
  ) %>%
  select(Category, occupation_grouped, N, Percent) %>%
  rename(Result = occupation_grouped)
Data$field_grouped <- dplyr::case_when(
  grepl("Law", Data$field, ignore.case = TRUE) ~ "Law",
  grepl("Economics|Finance|Business|BWL|Controlling", Data$field, ignore.case = TRUE) ~ "Economics",
  grepl("Psychology|Sociology|Social|Education|Political", Data$field, ignore.case = TRUE) ~ "Social sciences",
  grepl("Humanities|Literature|Philosophy|History|Language|Media", Data$field, ignore.case = TRUE) ~ "Humanities",
  grepl("Biology|Physics|Chemistry|Natural|Geo|Math", Data$field, ignore.case = TRUE) ~ "Natural sciences",
  grepl("Computer|Data|Informatik|IT|Coding|Programming|AI", Data$field, ignore.case = TRUE) ~ "Computer Science",
  grepl("Medicine|Health|Public Health|Nursing", Data$field, ignore.case = TRUE) ~ "Medicine",
  TRUE ~ "Other"
)
field_summary <- Data %>% 
  filter(!is.na(field_grouped)) %>% 
  count(field_grouped, sort = TRUE) %>%
  slice(1) %>%
  mutate(
    Category = "Most Common Field",
    N = n,
    Percent = round(n / n_total2 * 100, 1)
  ) %>%
  select(Category, field_grouped, N, Percent) %>%
  rename(Result = field_grouped)
demographic_summary2 <- bind_rows(
  gender_summary,
  education_summary,
  occupation_summary,
  field_summary
)
kable(demographic_summary2, caption = "Demographic Summary")
```
## Individual group demographics
```{r}
numeric_group_summary <- Data %>%
  group_by(group) %>%
  summarise(
    Age_Mean = round(mean(age, na.rm = TRUE), 1),
    Age_Median = median(age, na.rm = TRUE),
    Age_Min = min(age, na.rm = TRUE),
    Age_Max = max(age, na.rm = TRUE),
    N_Age = sum(!is.na(age)),
    Confidence_Mean = round(mean(estimation_confidence, na.rm = TRUE), 2),
    Confidence_Median = median(estimation_confidence, na.rm = TRUE),
    Confidence_Min = min(estimation_confidence, na.rm = TRUE),
    Confidence_Max = max(estimation_confidence, na.rm = TRUE),
    N_Confidence = sum(!is.na(estimation_confidence))
  )
kable(numeric_group_summary, caption = "Numeric Demographics (Age and Estimation Confidence) by Group")
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}
categorical_group_summary <- Data %>%
  group_by(group) %>%
  summarise(
    Most_Common_Gender = get_mode(gender),
    Most_Common_Education = get_mode(education_grouped),
    Most_Common_Occupation = get_mode(occupation_grouped),
    Most_Common_Field = get_mode(field_grouped),
    Gender_Count = sum(!is.na(gender)),
    Education_Count = sum(!is.na(education_grouped)),
    Occupation_Count = sum(!is.na(occupation_grouped)),
    Field_Count = sum(!is.na(field_grouped))
  )
kable(categorical_group_summary, caption = "Most Common Categorical Demographics by Group")
```

## Plots for demographic questions
```{r}
ggplot(Data[!is.na(Data$age), ], aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "How old are you?", x = "Age", y = "Frequency") +
  theme_minimal()
ggplot(Data, aes(x = gender)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Which gender do you identify with?", x = "Gender", y = "Count") +
  theme_minimal()
ggplot(Data, aes(x = education_grouped)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "What is your current education level?", x = "Education", y = "Count") +
  theme_minimal()
ggplot(Data, aes(x = occupation_grouped)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "What are you currently doing full time?", x = "Occupation", y = "Count") +
  theme_minimal()
ggplot(Data, aes(x = field_grouped)) +
  geom_bar(fill = "orchid", color = "black") +
  labs(title = "What is your current field of study or work?", x = "Field", y = "Count") +
  theme_minimal()
ggplot(Data, aes(x = estimation_confidence)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "How confident are you in your numerical estimations?", x = "Confidence (1= not at all – 7 = very confident)", y = "Count") +
  theme_minimal()
```
## Table for Anchoring Questions
```{r}
anchor_vars <- c("gandhi_age", "guess_africa", "guess_groceries", 
                 "guess_income", "guess_apples", "guess_inflation", "guess_saving")
long_summary <- lapply(anchor_vars, function(var) {
  Data %>%
    filter(group %in% c("High", "Low", "Control")) %>%
    group_by(group) %>%
    summarise(
      Variable = var,
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      N_Valid = sum(!is.na(.data[[var]])),
      .groups = "drop"
    )
}) %>%
  bind_rows() %>%
  pivot_wider(names_from = group, values_from = c(Mean, SD, N_Valid))
colnames(long_summary) <- gsub("_(High|Low|Control)", " (\\1)", colnames(long_summary))
kable(long_summary, caption = "Summary Statistics for All Anchor Guess Variables by Group")
```

## Plots for Anchoring Questions
```{r}
ggplot(Data %>% filter(gandhi_age < 120), aes(x = group, y = gandhi_age)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "How old was Mahatma Gandhi when he died?", x = "Group", y = "Estimated Age") +
  theme_minimal()
ggplot(Data %>% filter(guess_africa <= 100), aes(x = group, y = guess_africa)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_y_sqrt() +  # or use scale_y_log10() if no zero values
  labs(title = "What is your best estimate of the percentage of African Countries in the UN? (√-scaled)", x = "Group", y = "Estimated %") +
  theme_minimal()
ggplot(Data %>% filter(guess_groceries < 200), aes(x = group, y = guess_groceries)) +
  geom_boxplot(fill = "wheat", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "What is the average price of a weekly grocery basket for one person? (€)", x = "Group", y = "Estimated Cost (€)") +
  theme_minimal()
ggplot(Data %>% filter(guess_income < 10000), aes(x = group, y = guess_income)) +
  geom_boxplot(fill = "plum", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "What is the average monthly net income (income after taxes) in Germany? (€)", x = "Group", y = "Estimated Income (€)") +
  theme_minimal()
ggplot(Data %>% filter(guess_apples < 10), aes(x = group, y = guess_apples)) +
  geom_boxplot(fill = "tomato", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "How much does a kilogram of apples in a supermarket cost? (€)", x = "Group", y = "Estimated Price (€)") +
  theme_minimal()
ggplot(Data %>% filter(guess_inflation < 0.2), aes(x = group, y = guess_inflation)) +
  geom_boxplot(fill = "gold", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "What is your estimate of the monthly inflation rate in Germany?", x = "Group", y = "Estimated Rate") +
  theme_minimal()
ggplot(Data %>% filter(guess_saving < 100), aes(x = group, y = guess_saving)) +
  geom_boxplot(fill = "darkseagreen", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  labs(title = "What percentage of your monthly income would you ideally save?", x = "Group", y = "Saving Percentage") +
  theme_minimal()
```


## Plots and table for risk aversion, time preference and Demand Awareness
```{r}
most_common <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return("No data")
  names(sort(table(x), decreasing = TRUE))[1]
}
summary_table <- data.frame(
  Category = c(
    "Most Common Intertemporal Choice",
    "Most Common Risk Preference",
    "Demand Awareness - Yes (%)"
  ),
  Result = c(
    most_common(Data$lottery_choice),
    most_common(Data$risk_choice),
    round(mean(tolower(Data$demand_awareness) == "yes", na.rm = TRUE) * 100, 1)
  )
)
kable(summary_table, caption = "Summary of Intertemporal and Risk Preferences")
ggplot(Data, aes(x = lottery_choice)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Intertemporal Choice Distribution",
       x = "Choice",
       y = "Count") +
  theme_minimal()
ggplot(Data, aes(x = risk_choice)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Risk Preference Distribution",
       x = "Choice",
       y = "Count") +
  theme_minimal()
```

## T-Test and significance of results
```{r}
anchor_vars <- c("gandhi_age", "guess_africa", "guess_groceries", 
                 "guess_income", "guess_apples", "guess_inflation", "guess_saving")
Data$anchored <- ifelse(Data$group %in% c("High", "Low"), "Anchored", Data$group)
comparisons <- list(
  c("High", "Low"),
  c("High", "Control"),
  c("Low", "Control"),
  c("Anchored", "Control")
)
results <- list()
for (var in anchor_vars) {
  for (groups in comparisons) {
    group1 <- groups[1]
    group2 <- groups[2]
    
    group_col <- if ("Anchored" %in% groups) "anchored" else "group"
    
    sub_data <- Data %>%
      filter(.data[[group_col]] %in% c(group1, group2)) %>%
      filter(!is.na(.data[[var]]))
    
    if (length(unique(sub_data[[group_col]])) == 2) {
      values_g1 <- sub_data %>% filter(.data[[group_col]] == group1) %>% pull(.data[[var]])
      values_g2 <- sub_data %>% filter(.data[[group_col]] == group2) %>% pull(.data[[var]])
      
      if (length(values_g1) > 1 && length(values_g2) > 1) {
        # Run t-test
        t_result <- t.test(values_g1, values_g2)
        p_val <- t_result$p.value
        
        sig <- case_when(
          p_val < 0.001 ~ "***",
          p_val < 0.01  ~ "**",
          p_val < 0.05  ~ "*",
          p_val < 0.1   ~ ".",
          TRUE          ~ ""
        )
        
        results[[length(results) + 1]] <- data.frame(
          variable = var,
          comparison = paste(group1, "vs", group2),
          mean_group1 = round(mean(values_g1, na.rm = TRUE), 2),
          mean_group2 = round(mean(values_g2, na.rm = TRUE), 2),
          t_statistic = round(t_result$statistic, 3),
          p_value = round(p_val, 4),
          sig = sig
        )
      }
    }
  }
}
ttest_table <- bind_rows(results)
kable(ttest_table, caption = "T-Test Results with Anchored vs Control and Significance Levels")
```

## ANOVA Test for Anchoring Tasks
```{r}
anchor_vars <- c("gandhi_age", "guess_africa", "guess_groceries", 
                 "guess_income", "guess_apples", "guess_inflation", "guess_saving")
comparisons <- list(
  c("High", "Low"),
  c("High", "Control"),
  c("Low", "Control"),
  c("Anchored", "Control")
)
Data$anchored <- ifelse(Data$group %in% c("High", "Low"), "Anchored", Data$group)
anova_results <- list()
for (var in anchor_vars) {
  for (groups in comparisons) {
    group1 <- groups[1]
    group2 <- groups[2]
    
    group_column <- if ("Anchored" %in% groups) "anchored" else "group"
    
    sub_data <- Data %>%
      filter(.data[[group_column]] %in% c(group1, group2)) %>%
      filter(!is.na(.data[[var]]))
    
    if (length(unique(sub_data[[group_column]])) == 2) {
      model <- aov(as.formula(paste(var, "~", group_column)), data = sub_data)
      p_value <- summary(model)[[1]][["Pr(>F)"]][1]
      
      anova_results[[length(anova_results) + 1]] <- data.frame(
        Variable = var,
        Comparison = paste(group1, "vs", group2),
        ANOVA_p_value = round(p_value, 4),
        Significant = ifelse(p_value < 0.05, "Yes", "No")
      )
    }
  }
}
anova_table <- bind_rows(anova_results)
kable(anova_table, caption = "ANOVA Results for All Comparisons (High, Low, Control, Anchored)")
```

## Effect size and cohen's d
---
```{r}
anchor_vars <- c("gandhi_age", "guess_africa", "guess_groceries", 
                 "guess_income", "guess_apples", "guess_inflation", "guess_saving")
comparisons <- list(
  c("High", "Low"),
  c("High", "Control"),
  c("Low", "Control"),
  c("Anchored", "Control")  # custom group
)
Data$anchored <- ifelse(Data$group %in% c("High", "Low"), "Anchored", Data$group)
results <- list()
for (var in anchor_vars) {
  for (groups in comparisons) {
    group1 <- groups[1]
    group2 <- groups[2]
    
    current_group <- if ("Anchored" %in% groups) "anchored" else "group"
    
    sub_data <- Data %>%
      filter(.data[[current_group]] %in% c(group1, group2)) %>%
      filter(!is.na(.data[[var]]))
    
    if (length(unique(sub_data[[current_group]])) == 2) {
      g1_values <- sub_data %>% filter(.data[[current_group]] == group1) %>% pull(.data[[var]])
      g2_values <- sub_data %>% filter(.data[[current_group]] == group2) %>% pull(.data[[var]])
      
      if (length(g1_values) > 1 && length(g2_values) > 1 &&
          sd(g1_values) > 0 && sd(g2_values) > 0) {
        
        t_result <- t.test(g1_values, g2_values)
        p_val <- t_result$p.value
        
        sig_star <- case_when(
          p_val < 0.001 ~ "***",
          p_val < 0.01 ~ "**",
          p_val < 0.05 ~ "*",
          p_val < 0.1 ~ ".",
          TRUE ~ ""
        )
       
        pooled_sd <- sqrt((sd(g1_values)^2 + sd(g2_values)^2) / 2)
        d_val <- (mean(g1_values) - mean(g2_values)) / pooled_sd
        
        interpretation <- case_when(
          abs(d_val) < 0.2 ~ "Negligible",
          abs(d_val) < 0.5 ~ "Small",
          abs(d_val) < 0.8 ~ "Medium",
          TRUE ~ "Large"
        )
        
        results[[length(results) + 1]] <- data.frame(
          variable = var,
          comparison = paste(group1, "vs", group2),
          mean_group1 = round(mean(g1_values), 2),
          mean_group2 = round(mean(g2_values), 2),
          cohen_d = round(d_val, 3),
          magnitude = interpretation,
          p_value = round(p_val, 4),
          sig = sig_star
        )
      }
    }
  }
}

cohen_table <- bind_rows(results)
kable(cohen_table, caption = "Cohen's d, Significance, and Effect Size Interpretation for Anchoring Tasks")
```

## Anchoring Indices
```{r}
anchor_values <- data.frame(
  variable = c("gandhi_age", "guess_africa", "guess_groceries",
               "guess_income", "guess_apples", "guess_inflation", "guess_saving"),
  anchor_low = c(35, 10, 55, 1000, 1.5, 1, 5),
  anchor_high = c(90, 65, 115, 4000, 4.5, 8, 30)
)
get_median <- function(data, var, group) {
  val <- data[data$group == group, ][[var]]
  if (length(val) > 0 && !all(is.na(val))) median(val, na.rm = TRUE) else NA
}
compute_ai_all <- function(var, low_anchor, high_anchor) {
  high_median <- get_median(Data, var, "High")
  low_median <- get_median(Data, var, "Low")
  control_median <- get_median(Data, var, "Control")
  
  ai_overall <- if (!is.na(high_median) && !is.na(low_median)) {
    (high_median - low_median) / (high_anchor - low_anchor)
  } else { NA }

  ai_high <- if (!is.na(high_median) && !is.na(control_median) && high_anchor != control_median) {
    (high_median - control_median) / (high_anchor - control_median)
  } else { NA }

  ai_low <- if (!is.na(low_median) && !is.na(control_median) && low_anchor != control_median) {
    (low_median - control_median) / (low_anchor - control_median)
  } else { NA }

  return(c(ai_overall = ai_overall, ai_high = ai_high, ai_low = ai_low))
}
ai_matrix <- mapply(
  compute_ai_all,
  var = anchor_values$variable,
  low_anchor = anchor_values$anchor_low,
  high_anchor = anchor_values$anchor_high
)
ai_table <- data.frame(
  variable = anchor_values$variable,
  AI_Overall = ai_matrix["ai_overall", ],
  AI_High = ai_matrix["ai_high", ],
  AI_Low = ai_matrix["ai_low", ]
)
kable(ai_table, digits = 3,
      caption = "Anchoring Index per Variable (Overall, High, and Low Anchors Based on Medians)")
```
