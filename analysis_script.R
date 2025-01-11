
library(tidyr)
library(dplyr)
library(readr)
library(summarytools)
library(ggplot2)
library(gridExtra)
library(stringr)
library(Rtsne)
library(reshape2)
library(car)
library(gtsummary)

# Load the dataset
data_2022 <- read_csv("S:\\biostats_consulting_lab\\cleaned_2022_survey_dta.csv")
data_2024 <- read_csv("S:\\biostats_consulting_lab\\cleaned_2024_survey_dta.csv")
head(data_2022)

# Rename the specified variables and clean data_2022
data_2022_cleaned <- data_2022 %>%
  rename(
    dob = sec1_q1,
    gender = sec1_q4,
    highest_education = sec1_q5,
    employment_status = sec1_q6,
    marital_status = sec1_q7,
    household_income = sec1_q8,
    residence_area = sec1_q9,
    survey_location = sec1_q10,
    survey_duration = sec11_start,
    religious = sec11_q156,
    religion = sec11_q157,
    specified_other_religion = sec11_q157other,
    science_contradict = sec11_q158,
    science_or_religion = sec11_q159
  ) %>%
  select(-consent, -availability) %>%
  mutate(
    religion = case_when(
      religion %in% c("CCAP", "Traditional African religion") ~ "Other",
      religion %in% c("Seventh Day Adventist", "CCAP") ~ "Other Christian",
      religion == "Prefer not to answer" ~ "Prefer not to answer [do not read aloud]",
      TRUE ~ religion
    ),
    employment_status = if_else(is.na(employment_status), "Missing", employment_status)
  )

data_2022_cleaned <- data_2022_cleaned %>%
  mutate(across(c(religion,science_or_religion, science_contradict, religious), 
                ~ replace_na(., "Missing")))

survey_date_fixed <- as.Date("2022-01-01")

data_2022_cleaned <- data_2022_cleaned %>%
  mutate(
    age = as.numeric(difftime(survey_date_fixed, dob, units = "days")) / 365
  )

age_quantiles <- quantile(data_2022_cleaned$age, probs = seq(0, 1, 0.25), na.rm = TRUE)

data_2022_cleaned <- data_2022_cleaned %>%
  mutate(
    age_category = cut(
      age,
      breaks = age_quantiles,
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )


# Summarize the count for each quantile
age_summary <- data_2022_cleaned %>%
  group_by(age_category) %>%
  summarize(
    Count = n(),
    Min_Age = min(age, na.rm = TRUE),
    Max_Age = max(age, na.rm = TRUE)
  )

# Display the summary table
print(age_summary)

# Bar plot for the number of observations in each quantile
ggplot(age_summary, aes(x = age_category, y = Count, fill = age_category)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Observations by Age Quantile",
    x = "Age Quantile",
    y = "Count"
  ) +
  theme_minimal()

# Density plot to visualize the distribution of age
ggplot(data_2022_cleaned, aes(x = age, fill = age_category)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Age by Quantile",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()

# Boxplot to show the distribution of age in each quantile
ggplot(data_2022_cleaned, aes(x = age_category, y = age, fill = age_category)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Age by Quantile",
    x = "Age Quantile",
    y = "Age"
  ) +
  theme_minimal()


# Clean and rename specified variables in data_2024
data_2024_cleaned <- data_2024 %>%
  # Rename variables
  rename(
    caseid = caseid,
    response_status = response_1,
    response_by = response_2,
    dob = birthdate,
    highest_education = educ_level,
    employment_status = employ_status,
    people_speak_to_daily = number_people,
    household_income = hh_income,
    specified_other_religion = religion_oth,
    call_status = call_status
  ) %>%
  
  # Select only the relevant variables
  select(
    dob, caseid, response_status, response_by, gender, highest_education, marital_status, parent_guardian, 
    employment_status, work_industry, people_speak_to_daily, 
    household_income, residence_area, religion, 
    specified_other_religion, call_status, survey_date
  ) %>%
  
  # Clean data by re-coding and handling missing values
  mutate(
    # Re-code religion variable by grouping similar categories
    religion = case_when(
      religion %in% c("Seventh Day Adventists", "Apostolic/New Apostlic Church", "Church of Christ",
                      "Gospel/NewTestament/Injili Church", "Salvation Army Church", "Assembly of God Church",
                      "Roho Church", "Church of God", "Jehovah's Witness", "Legio Maria Church", "NENO",
                      "Repentance and Holiness", "Pentecostal/ Protestant Church") ~ "Other Christian",
      religion == "Prefer not to answer [do not read aloud]" ~ "Prefer not to answer",
      religion == "Akorino" ~ "Other",
      religion == "Baptist Church" ~ "Baptist",
      TRUE ~ religion
    ),
    
    # Re-code employment_status variable
    employment_status = case_when(
      employment_status %in% c("Self-employed (includes agribusiness)", "Peasant farmer") ~ "Self-employed",
      TRUE ~ employment_status
    ),
    highest_education = case_when(
      highest_education == "Prefer not to answer" ~ "Prefer not to answer [do not read aloud]",
      TRUE ~ highest_education
    )
  ) %>%
  
  # Replace NA values in highest_education with "Missing"
  mutate(highest_education = replace_na(highest_education, "Missing")) %>%
  mutate(marital_status = replace_na(marital_status, "Missing"))%>%
  mutate(parent_guardian = replace_na(parent_guardian, "Missing"))%>%
  mutate(work_industry = replace_na(work_industry, "Missing"))%>%
  mutate(people_speak_to_daily = replace_na(people_speak_to_daily, "Missing"))%>%
  mutate(household_income = replace_na(household_income, "Missing"))%>%
  mutate(residence_area = replace_na(residence_area, "Missing"))%>%
  mutate(employment_status = replace_na(employment_status, "Missing"))%>%
  mutate(religion = replace_na(religion, "Missing"))

  
data_2024_cleaned <- data_2024_cleaned %>%
  left_join(data_2022_cleaned %>% select(caseid, gender, religion, dob), by = "caseid", suffix = c("_2024", "_2022")) %>%
  mutate(
    # 如果2024的gender是NA，用2022的gender进行填充
    gender_2024 = coalesce(gender_2024, gender_2022),
    # 如果gender仍为NA，则替换为"Unknown"
    gender_2024 = replace_na(gender_2024, "Unknown"),
    
    dob_2024 = coalesce(dob_2024,dob_2022),
    # 如果religion仍为NA，则替换为"Unknown"
    dob_2024 = replace_na(dob_2024, "Unknown")
  ) %>%
  
  # 移除gender_2022和religion_2022，保留处理过的gender_2024和religion_2024
  select(-gender_2022, -dob_2022) %>%
  rename(gender = gender_2024, dob = dob_2024)


# Replace "Prefer not to answer [do not read aloud]" with "Prefer not to answer" across all columns
data_2024_cleaned <- data_2024_cleaned %>%
  mutate(across(everything(), ~str_replace(., "Prefer not to answer \\[do not read aloud\\]", "Prefer not to answer")))
data_2022_cleaned <- data_2022_cleaned %>%
  mutate(across(everything(), ~str_replace(., "Prefer not to answer \\[do not read aloud\\]", "Prefer not to answer")))

# Display the first few rows of the cleaned datasets
head(data_2022_cleaned)
head(data_2024_cleaned)

# Check for missing values in both datasets
missing_values_2022 <- sapply(data_2022_cleaned, function(x) sum(is.na(x)))
missing_values_2024 <- sapply(data_2024_cleaned, function(x) sum(is.na(x)))
print(missing_values_2022)
print(missing_values_2024)

# Get summary statistics for both datasets
summary(data_2022_cleaned)
summary(data_2024_cleaned)

# Check case IDs in both datasets
caseid_2022 <- data_2022_cleaned$caseid
caseid_2024 <- data_2024_cleaned$caseid

# Filter the 2024 dataset to only include those who successfully followed up
successful_followup_2024 <- data_2024_cleaned %>%
  filter(response_status == "Answered the phone, correct respondent" & call_status == "Completed")

# Extract the case IDs of the successfully followed-up participants
caseid_successful_followup <- successful_followup_2024$caseid

# Identify participants present in both 2022 and successfully followed up in 2024
common_successful_followup <- intersect(caseid_2022, caseid_successful_followup)

# Identify participants in 2022 but not in the successfully followed-up group in 2024 (dropped out)
dropped_participants <- setdiff(caseid_2022, caseid_successful_followup)

# Identify participants in 2024 (successfully followed up) but not in 2022 (new participants)
new_participants <- setdiff(caseid_successful_followup, caseid_2022)

# Output the counts
cat("Number of participants successfully followed up in 2024: ", length(common_successful_followup), "\n")
cat("Number of participants who dropped out after 2022: ", length(dropped_participants), "\n")
cat("Number of new participants who joined in 2024: ", length(new_participants), "\n")

# View unique values for key variables across both datasets
list(
  religion_2022 = unique(data_2022_cleaned$religion),
  religion_2024 = unique(data_2024_cleaned$religion_2024),
  highest_education_2022 = unique(data_2022_cleaned$highest_education),
  highest_education_2024 = unique(data_2024_cleaned$highest_education),
  employment_status_2022 = unique(data_2022_cleaned$employment_status),
  employment_status_2024 = unique(data_2024_cleaned$employment_status),
  marital_status_2022 = unique(data_2022_cleaned$marital_status),
  marital_status_2024 = unique(data_2024_cleaned$marital_status)
)

data_2022_cleaned <- data_2022_cleaned %>%
  semi_join(data_2024_cleaned, by = "caseid")  # 仅保留2024中存在的caseid

data_2024_cleaned <- data_2024_cleaned %>%
  semi_join(data_2022_cleaned, by = "caseid")  # 仅保留2022中存在的caseid

# Function to calculate percentage of missing values
calculate_missing_percentage <- function(data) {
  data %>%
    summarise(across(everything(), ~ sum(. == "Missing") / n() * 100)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_percentage")
}

# Calculate missing percentages for both datasets
missing_2022 <- calculate_missing_percentage(data_2022_cleaned)
missing_2024 <- calculate_missing_percentage(data_2024_cleaned)

# Plot missing values for data_2022_cleaned
plot_2022 <- ggplot(missing_2022, aes(x = reorder(variable, -missing_percentage), y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Missing in data_2022", x = "Variables", y = "Missing Percentage (%)") +
  theme_minimal()

# Plot missing values for data_2024_cleaned
plot_2024 <- ggplot(missing_2024, aes(x = reorder(variable, -missing_percentage), y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Missing in data_2024", x = "Variables", y = "Missing Percentage (%)") +
  theme_minimal()

# Combine the two plots into one, displayed side by side
combined_plot <- grid.arrange(plot_2022, plot_2024, ncol = 2)

# Display the combined plot
combined_plot

# Merge the datasets by caseid and create new variables indicating changes between 2022 and 2024
merged_data <- full_join(data_2022_cleaned, data_2024_cleaned, by = "caseid", suffix = c("_2022", "_2024")) %>%
  mutate(lost = if_else(is.na(response_status) | response_status != "Answered the phone, correct respondent", 1, 0)) %>%
  mutate(
    education_change = if_else(
      is.na(highest_education_2022) | is.na(highest_education_2024) | 
      highest_education_2022 == "Missing" | highest_education_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(highest_education_2022 != highest_education_2024, 1, 0)
    ),
    
    employment_change = if_else(
      is.na(employment_status_2022) | is.na(employment_status_2024) | 
      employment_status_2022 == "Missing" | employment_status_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(employment_status_2022 != employment_status_2024, 1, 0)
    ),
    
    income_change = if_else(
      is.na(household_income_2022) | is.na(household_income_2024) | 
      household_income_2022 == "Missing" | household_income_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(household_income_2022 != household_income_2024, 1, 0)
    ),
    
    residence_change = if_else(
      is.na(residence_area_2022) | is.na(residence_area_2024) | 
      residence_area_2022 == "Missing" | residence_area_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(residence_area_2022 != residence_area_2024, 1, 0)
    ),
    
    religion_change = if_else(
      is.na(religion_2022) | is.na(religion_2024) | 
      religion_2022 == "Missing" | religion_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(religion_2022 != religion_2024, 1, 0)
    ),
    
    residence_area_change = if_else(
      is.na(residence_area_2022) | is.na(residence_area_2024) | 
      residence_area_2022 == "Missing" | residence_area_2024 == "Missing", 
      3,  # Set as 3 when missing in either year
      if_else(residence_area_2022 != residence_area_2024, 1, 0)
    )
  ) %>%
  select(
    caseid, 
    dob_2022, gender_2022, 
    highest_education_2022, highest_education_2024, 
    marital_status_2022, marital_status_2024, 
    employment_status_2022, employment_status_2024, 
    household_income_2022, household_income_2024, 
    residence_area_2022, residence_area_2024, residence_area_change,
    religion_2022, religion_2024, 
    specified_other_religion_2022,
    response_status, response_by, 
    parent_guardian, science_or_religion,
    lost, religious,
    education_change, employment_change, income_change, residence_change, religion_change
  )

# View the first few rows to verify the new order
head(merged_data)

dfSummary(merged_data) %>% view()

# Summarize the percentages of each change status (0, 1, 3) for each variable
change_percentages <- merged_data %>%
  summarize(
    education_change_0 = mean(education_change == 0, na.rm = TRUE) * 100,  # No change
    education_change_1 = mean(education_change == 1, na.rm = TRUE) * 100,  # Changed
    education_change_3 = mean(education_change == 3, na.rm = TRUE) * 100,  # Unknown

    employment_change_0 = mean(employment_change == 0, na.rm = TRUE) * 100,
    employment_change_1 = mean(employment_change == 1, na.rm = TRUE) * 100,
    employment_change_3 = mean(employment_change == 3, na.rm = TRUE) * 100,

    income_change_0 = mean(income_change == 0, na.rm = TRUE) * 100,
    income_change_1 = mean(income_change == 1, na.rm = TRUE) * 100,
    income_change_3 = mean(income_change == 3, na.rm = TRUE) * 100,

    residence_change_0 = mean(residence_change == 0, na.rm = TRUE) * 100,
    residence_change_1 = mean(residence_change == 1, na.rm = TRUE) * 100,
    residence_change_3 = mean(residence_change == 3, na.rm = TRUE) * 100,
    
    religion_change_0 = mean(religion_change == 0, na.rm = TRUE) * 100,
    religion_change_1 = mean(religion_change == 1, na.rm = TRUE) * 100,
    religion_change_3 = mean(religion_change == 3, na.rm = TRUE) * 100,
    
    residence_area_change_0 = mean(residence_area_change == 0, na.rm = TRUE) * 100,  # No change in residence area
    residence_area_change_1 = mean(residence_area_change == 1, na.rm = TRUE) * 100,  # Changed residence area
    residence_area_change_3 = mean(residence_area_change == 3, na.rm = TRUE) * 100   # Unknown/missing residence area
  )

change_percentages

dfSummary(merged_data) %>% view()

# Prepare data for pie charts
education_data <- merged_data %>%
  count(education_change) %>%
  mutate(percentage = n / sum(n) * 100)

employment_data <- merged_data %>%
  count(employment_change) %>%
  mutate(percentage = n / sum(n) * 100)

income_data <- merged_data %>%
  count(income_change) %>%
  mutate(percentage = n / sum(n) * 100)

residence_data <- merged_data %>%
  count(residence_change) %>%
  mutate(percentage = n / sum(n) * 100)

religion_data <- merged_data %>%
  count(religion_change) %>%
  mutate(percentage = n / sum(n) * 100)

residence_area_data <- merged_data %>%
  count(residence_area_change) %>%
  mutate(percentage = n / sum(n) * 100)

# Create pie charts for each change status
education_pie <- ggplot(education_data, aes(x = "", y = percentage, fill = factor(education_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Education Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

employment_pie <- ggplot(employment_data, aes(x = "", y = percentage, fill = factor(employment_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Employment Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

income_pie <- ggplot(income_data, aes(x = "", y = percentage, fill = factor(income_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Income Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

residence_pie <- ggplot(residence_data, aes(x = "", y = percentage, fill = factor(residence_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Residence Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

religion_pie <- ggplot(religion_data, aes(x = "", y = percentage, fill = factor(religion_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Religion Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

residence_area_pie <- ggplot(residence_area_data, aes(x = "", y = percentage, fill = factor(residence_area_change))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Residence Area Change Status", fill = "Status", y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green", "orange", "red"), 
                    labels = c("No Change", "Changed", "Missing"))

# Arrange the pie charts in a 2 x 3 layout
grid.arrange(education_pie, employment_pie, income_pie, residence_pie, religion_pie, residence_area_pie, ncol = 3)
                              
# Split data into lost and followed groups
lost_data <- merged_data %>% filter(lost == 1) 
followed_data <- merged_data %>% filter(lost == 0)

# Function to prepare data for pie chart
prepare_pie_data <- function(data, variable) {
  data %>%
    count({{ variable }}) %>%
    mutate(percentage = n / sum(n) * 100)
}

# Function to create pie chart
create_pie_chart <- function(pie_data, title, variable_name) {
  ggplot(pie_data, aes(x = "", y = percentage, fill = factor({{ variable_name }}))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = title, fill = "Status", y = "", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_blank()) +
    scale_fill_manual(values = c("green", "orange", "red"), 
                      labels = c("No Change", "Changed", "Missing"))
}

# Prepare data for pie charts for both groups
# For Lost Group
education_lost <- prepare_pie_data(lost_data, education_change)
employment_lost <- prepare_pie_data(lost_data, employment_change)
income_lost <- prepare_pie_data(lost_data, income_change)
residence_lost <- prepare_pie_data(lost_data, residence_change)
religion_lost <- prepare_pie_data(lost_data, religion_change)
residence_area_lost <- prepare_pie_data(lost_data, residence_area_change)

# For Followed Group
education_followed <- prepare_pie_data(followed_data, education_change)
employment_followed <- prepare_pie_data(followed_data, employment_change)
income_followed <- prepare_pie_data(followed_data, income_change)
residence_followed <- prepare_pie_data(followed_data, residence_change)
religion_followed <- prepare_pie_data(followed_data, religion_change)
residence_area_followed <- prepare_pie_data(followed_data, residence_area_change)

# Create pie charts for lost group
education_pie_lost <- create_pie_chart(education_lost, "Education Change", education_change)
employment_pie_lost <- create_pie_chart(employment_lost, "Employment Change", employment_change)
income_pie_lost <- create_pie_chart(income_lost, "Income Change", income_change)
residence_pie_lost <- create_pie_chart(residence_lost, "Residence Change", residence_change)
religion_pie_lost <- create_pie_chart(religion_lost, "Religion Change", religion_change)
residence_area_pie_lost <- create_pie_chart(residence_area_lost, "Residence Area Change", residence_area_change)

# Create pie charts for followed group
education_pie_followed <- create_pie_chart(education_followed, "Education Change", education_change)
employment_pie_followed <- create_pie_chart(employment_followed, "Employment Change", employment_change)
income_pie_followed <- create_pie_chart(income_followed, "Income Change", income_change)
residence_pie_followed <- create_pie_chart(residence_followed, "Residence Change", residence_change)
religion_pie_followed <- create_pie_chart(religion_followed, "Religion Change", religion_change)
residence_area_pie_followed <- create_pie_chart(residence_area_followed, "Residence Area Change", residence_area_change)

# Arrange the pie charts in two sets (Lost and Followed)
# Lost group: 2 rows x 3 columns
grid.arrange(education_pie_lost, employment_pie_lost, income_pie_lost, 
             residence_pie_lost, religion_pie_lost, residence_area_pie_lost, 
             ncol = 3, top = "Lost Group")

# Followed group: 2 rows x 3 columns
grid.arrange(education_pie_followed, employment_pie_followed, income_pie_followed, 
             residence_pie_followed, religion_pie_followed, residence_area_pie_followed, 
             ncol = 3, top = "Followed Group")

# Convert categorical variables to factors
merged_data$highest_education_2022 <- as.factor(merged_data$highest_education_2022)
merged_data$employment_status_2022 <- as.factor(merged_data$employment_status_2022)
merged_data$household_income_2022 <- as.factor(merged_data$household_income_2022)
merged_data$residence_area_2022 <- as.factor(merged_data$residence_area_2022)
merged_data$gender_2022 <- as.factor(merged_data$gender_2022)
merged_data$marital_status_2022 <- as.factor(merged_data$marital_status_2022)
merged_data$religion_2022 <- as.factor(merged_data$religion_2022)
merged_data$lost <- as.factor(merged_data$lost)
merged_data$science_or_religion <- as.factor(merged_data$science_or_religion)

# Create a new dataframe by removing rows containing "Missing" or "Prefer not to answer"
clean_data <- merged_data %>%
  filter(
    !highest_education_2022 %in% c("Missing") &
    !employment_status_2022 %in% c("Missing") &
    !household_income_2022 %in% c("Missing") &
    !residence_area_2022 %in% c("Missing") &
    !gender_2022 %in% c("Missing") &
    !marital_status_2022 %in% c("Missing") &
    !religion_2022 %in% c("Missing")
  )

# Remove the specified_other_religion_2022 and response_by columns
clean_data <- clean_data %>%
  select(-specified_other_religion_2022, -response_by)

# Check the cleaned data
print(dim(clean_data))  # Check the number of rows and columns

clean_model <- glm(lost ~ highest_education_2022 + employment_status_2022 + household_income_2022 + residence_area_2022 + gender_2022 + marital_status_2022 + religion_2022, data = clean_data, family = binomial)
summary(clean_model)

simplified_model <- glm(lost ~ employment_status_2022 + gender_2022 + religion_2022, 
                        data = clean_data, family = binomial)
summary(simplified_model)

chi_square_test <- anova(simplified_model, clean_model, test = "Chisq")
print(chi_square_test)

# Generate predicted probabilities and predicted classes
predicted_probs <- predict(simplified_model, type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)

# Confusion matrix
confusion_matrix <- table(Predicted = predicted_class, Actual = clean_data$lost)
print("Confusion Matrix:")
print(confusion_matrix)

# Plot ROC curve and calculate AUC
library(pROC)
roc_curve <- roc(clean_data$lost, predicted_probs)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Output McFadden's R^2
null_model <- glm(lost ~ 1, data = clean_data, family = binomial)  # Null model
r2_mcfadden <- 1 - (logLik(clean_model) / logLik(null_model))
cat("McFadden's R^2:", r2_mcfadden, "\n")

library(nnet)
multinom_model <- multinom(lost ~ highest_education_2022 + employment_status_2022 + household_income_2022 + 
                   residence_area_2022 + gender_2022 + marital_status_2022 + religion_2022, data = clean_data)
summary(multinom_model)

# Convert the 'lost' variable to integer type
clean_data$lost <- as.integer(clean_data$lost)

poisson_model <- glm(lost ~ highest_education_2022 + employment_status_2022 + household_income_2022 + 
                   residence_area_2022 + gender_2022 + marital_status_2022 + religion_2022, 
                     data = clean_data, family = poisson)
summary(poisson_model)

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the categorical variables for conversion to dummy variables
categorical_vars <- c("household_income_2022", "highest_education_2022", "employment_status_2022", 
                      "residence_area_2022", "gender_2022", "marital_status_2022", "religion_2022")

# Combine Lost and Followed groups first and add group label
combined_data <- merged_data %>%
  mutate(group = if_else(lost == 1, "Lost", "Followed")) %>%
  select(all_of(categorical_vars), group) %>%
  filter(!if_any(all_of(categorical_vars), ~ . == "Missing"))

# Convert categorical variables to factors
combined_data <- combined_data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Apply model.matrix to the combined dataset (convert categorical variables to dummy variables)
combined_data_clean <- model.matrix(~ . - 1, data = combined_data) %>%
  as.data.frame()

# Add group column back to the cleaned data
combined_data_clean$group <- combined_data$group

# Remove the group column before running PCA
combined_data_for_pca <- combined_data_clean %>%
  select(-group)

# Remove columns with zero variance (constant columns)
combined_data_for_pca <- combined_data_for_pca[, apply(combined_data_for_pca, 2, var) != 0]

# Perform PCA on the cleaned data, scaling the variables
combined_pca <- prcomp(scale(combined_data_for_pca), center = TRUE, scale. = TRUE)

# Extract the first two principal components and add group labels back
pca_df <- as.data.frame(combined_pca$x[, 1:2])
pca_df$group <- combined_data_clean$group

# Plot the PCA results using ggplot2
ggplot(pca_df, aes(x = PC1, y = PC2, color = group)) +
  geom_point() +
  labs(title = "PCA: Lost vs Followed Groups", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Initial setup for categorical variables
categorical_vars <- c("household_income_2022", "highest_education_2022", "employment_status_2022", 
                      "residence_area_2022", "gender_2022", "marital_status_2022", "religion_2022")

# Combine Lost and Followed groups first and add group label
combined_data <- merged_data %>%
  mutate(group = if_else(lost == 1, "Lost", "Followed")) %>%
  select(all_of(categorical_vars), group) %>%
  filter(!if_any(all_of(categorical_vars), ~ . == "Missing"))

# Convert categorical variables to factors
combined_data <- combined_data %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Apply model.matrix to the combined dataset (with consistent dummy variables for both groups)
combined_data_clean <- model.matrix(~ . - 1, data = combined_data) %>%
  as.data.frame()

# Add group column back to the cleaned data
combined_data_clean$group <- combined_data$group

# Remove the group column before running PCA
combined_data_for_pca <- combined_data_clean %>%
  select(-group)

# Identify and remove columns with zero variance
combined_data_for_pca <- combined_data_for_pca[, apply(combined_data_for_pca, 2, var) != 0]

# 1. Standardize the data
combined_data_for_pca_scaled <- scale(combined_data_for_pca)

# 2. Perform PCA on the scaled data
combined_pca_scaled <- prcomp(combined_data_for_pca_scaled, center = TRUE, scale. = TRUE)

# 3. Calculate explained variance
explained_variance <- combined_pca_scaled$sdev^2 / sum(combined_pca_scaled$sdev^2)

# 4. Plot the explained variance for each principal component
explained_variance_df <- data.frame(
  PC = seq_along(explained_variance),
  Variance = explained_variance
)

ggplot(explained_variance_df, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity") +
  labs(title = "Explained Variance by Principal Components", x = "Principal Component", y = "Variance Explained") +
  theme_minimal()

# 5. Extract the first two principal components and plot PCA
pca_df_scaled <- as.data.frame(combined_pca_scaled$x[, 1:2])
pca_df_scaled$group <- combined_data_clean$group

# Plot PCA results
ggplot(pca_df_scaled, aes(x = PC1, y = PC2, color = group)) +
  geom_point() +
  labs(title = "Scaled PCA: Lost vs Followed Groups", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# 6. Identify outliers based on PCA results
outliers <- pca_df_scaled %>%
  filter(PC1 < -10 | PC2 < -10)

# 7. Remove outliers and re-plot PCA without outliers
pca_df_cleaned <- pca_df_scaled %>%
  filter(PC1 > -10 & PC2 > -10)  # Assuming -10 is the threshold for outliers

# Re-plot PCA without outliers
ggplot(pca_df_cleaned, aes(x = PC1, y = PC2, color = group)) +
  geom_point() +
  labs(title = "PCA without Outliers: Lost vs Followed Groups", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Calculate the proportion of variance explained by each component
explained_variance <- combined_pca_scaled$sdev^2 / sum(combined_pca_scaled$sdev^2)

# Calculate the cumulative explained variance
cumulative_explained_variance <- cumsum(explained_variance)

# Plot the cumulative explained variance
plot(cumulative_explained_variance, type = "b", xlab = "Number of Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Explained Variance by Principal Components")

# Add a horizontal line for 80% explained variance
abline(h = 0.80, col = "red", lty = 2)
abline(h = 0.90, col = "blue", lty = 2)

# Determine how many components explain at least 80% variance
components_80 <- which(cumulative_explained_variance >= 0.80)[1]
components_90 <- which(cumulative_explained_variance >= 0.90)[1]

print(paste("Number of components to retain for 80% variance:", components_80))
print(paste("Number of components to retain for 90% variance:", components_90))

# Convert categorical variables to dummy variables and remove rows containing "Missing"
combined_data <- merged_data %>%
  mutate(group = if_else(lost == 1, "Lost", "Followed")) %>%
  select(all_of(categorical_vars), group) %>%
  filter(!if_any(all_of(categorical_vars), ~ . == "Missing")) %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Apply model.matrix to convert the categorical variables into dummy variables
combined_data_clean <- model.matrix(~ . - 1, data = combined_data) %>%
  as.data.frame()

# Remove duplicate rows before running t-SNE
combined_data_clean <- combined_data_clean %>%
  distinct()

# Extract the group information for later plotting
group_labels <- combined_data$group[1:nrow(combined_data_clean)]  # Ensure it matches the reduced dataset size

# Perform t-SNE on the dummy variables, setting a perplexity value (typically between 5 and 50)
set.seed(42)  # Set seed for reproducibility
tsne_results <- Rtsne(as.matrix(combined_data_clean), dims = 2, perplexity = 10, verbose = TRUE, max_iter = 1000)

# Convert t-SNE results into a data frame for plotting
tsne_df <- as.data.frame(tsne_results$Y)
colnames(tsne_df) <- c("Dim1", "Dim2")
tsne_df$group <- group_labels

# Plot the t-SNE results using ggplot2
ggplot(tsne_df, aes(x = Dim1, y = Dim2, color = group)) +
  geom_point() +
  labs(title = "t-SNE: Lost vs Followed Groups", x = "t-SNE Dimension 1", y = "t-SNE Dimension 2") +
  theme_minimal()

data_2022_cleaned[data_2022_cleaned == "Missing"] <- NA
data_2024_cleaned[data_2024_cleaned== "Missing"] <- NA
dfSummary(data_2022_cleaned) %>% view()

# Replace "Missing" with NA in the 2022 dataset only
data_2022_cleaned[data_2022_cleaned == "Missing"] <- NA

data_2022_cleaned <- data_2022_cleaned[, !names(data_2022_cleaned) %in% c("science_or_religion", "survey_duration")]

# Convert specified columns to factors in the 2022 dataset
data_2022_cleaned <- within(data_2022_cleaned, {
    highest_education = as.factor(highest_education)
    employment_status = as.factor(employment_status)
    household_income = as.factor(household_income)
    residence_area = as.factor(residence_area)
    gender = as.factor(gender)
    marital_status = as.factor(marital_status)
    religion = as.factor(religion)
    science_contradict = as.factor(science_contradict)
    religious = as.factor(religious)
})
library(mice)

method <- make.method(data_2022_cleaned)
method["highest_education"] <- "polyreg"
method["employment_status"] <- "polyreg"
method["household_income"] <- "polyreg"
method["residence_area"] <- "polyreg"
method["gender"] <- "logreg"         # binary categorical
method["marital_status"] <- "polyreg"
method["religion"] <- "polyreg"
method["science_contradict"] <- "polyreg"
method["religious"] <- "polyreg"

# max_it = 100
imputed_data_2022 <- mice(data_2022_cleaned, m = 5, method = method, maxit = 10, seed = 500)
completed_data_2022 <- complete(imputed_data_2022, 1)

head(completed_data_2022)
dfSummary(completed_data_2022) %>% view()

          
library(dplyr)
library(tidyr)
library(ggplot2)

calculate_missing_percentage <- function(data) {
  data %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_percentage")
}

missing_2022 <- calculate_missing_percentage(completed_data_2022)

ggplot(missing_2022, aes(x = reorder(variable, -missing_percentage), y = missing_percentage)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Missing in data_2022", x = "Variables", y = "Missing Percentage (%)") +
  theme_minimal()

# First, extract case IDs from 2024 data for those who answered correctly and completed the survey
followed_case_ids <- data_2024_cleaned %>%
  filter(response_status == "Answered the phone, correct respondent") %>%
  pull(caseid)

# Then, create the `if_follow` variable in the 2022 data based on participation in both surveys
completed_data_2022 <- completed_data_2022 %>%
  mutate(if_follow = if_else(caseid %in% followed_case_ids, 1, 0))

# Ensure categorical variables are factors
completed_data_2022 <- completed_data_2022 %>%
  mutate(
    highest_education = as.factor(highest_education),
    employment_status = as.factor(employment_status),
    household_income = as.factor(household_income),
    residence_area = as.factor(residence_area),
    gender = as.factor(gender),
    marital_status = as.factor(marital_status),
    religion = as.factor(religion),
    age_category = as.factor(age_category)
  )

# Logistic regression model to predict if a participant is followed up
logistic_followup_model1 <- glm(
  if_follow ~ highest_education + employment_status + household_income +
    residence_area + gender + marital_status + religion + age_category,
  data = completed_data_2022,
  family = binomial
)

# Display the summary of the logistic regression model
summary(logistic_followup_model1)

model_summary <- summary(logistic_followup_model1)
coef_table <- model_summary$coefficients
p_values <- coef_table[, "Pr(>|z|)"]
adjusted_p_values <- p.adjust(p_values, method = "BH")
coef_table <- cbind(coef_table, adjusted_p_values)
coef_table

# Create 'follow_status' variable in data_2024_cleaned
data_2024_cleaned <- data_2024_cleaned %>%
  mutate(follow_status = case_when(
    call_status %in% c("Completed", "Answered, but not completed/Appointment", "Refusal") ~ "followed",
    call_status %in% c("Answered, but not by the respondent", "No answer", "Not Eligible",
                       "Respondent Hung Phone", "Deceased", "Line out of Service") ~ "lost",
    TRUE ~ NA_character_  # Assign NA to any other call_status values
  ))
# iDs of participants followed
followed_case_ids <- data_2024_cleaned %>%
  filter(follow_status == "followed") %>%
  pull(caseid)

completed_data_2022 <- completed_data_2022 %>%
  mutate(if_follow2 = if_else(caseid %in% followed_case_ids, 1, 0))

# Logistic regression model to predict if a participant is followed up
logistic_followup_model2 <- glm(
  if_follow2 ~ highest_education + employment_status + household_income +
    residence_area + gender + marital_status + religion + age_category,
  data = completed_data_2022,
  family = binomial
)
summary(logistic_followup_model2)

model_summary <- summary(logistic_followup_model2)
coef_table <- model_summary$coefficients
p_values <- coef_table[, "Pr(>|z|)"]
adjusted_p_values <- p.adjust(p_values, method = "BH")
coef_table <- cbind(coef_table, adjusted_p_values)
coef_table

library(broom)
library(dplyr)
library(ggplot2)

term_order <- c(
  # Age
  "age_categoryQ1",
  "age_categoryQ2",
  "age_categoryQ3",
  "age_categoryQ4",
  
  # Gender
  "genderFemale",
  "genderMale",
  
  # Marital Status
  "marital_statusSingle",
  "marital_statusMarried",
  "marital_statusDivorced/Separated",
  "marital_statusWidowed",
  "marital_statusPrefer not to answer",
  
  # Religion
  "religionMuslim",
  "religionBaptist",
  "religionCatholic",
  "religionOther Christian",
  "religionOther",
  "religionPrefer not to answer",
  
  # Highest Education
  "highest_educationNo school/Did not complete primary",
  "highest_educationPrimary",
  "highest_educationSecondary",
  
  # Household Income
  "household_incomeAllowed me to save just a little",
  "household_incomeOnly just met my expenses",
  "household_incomeWas not sufficient, so needed to use savings to meet expenses",
  "household_incomeWas really not sufficient, so needed to borrow to meet expenses",
  "household_incomePrefer not to answer",
  
  # Employment Status
  "employment_statusEmployed full-time",
  "employment_statusEmployed part-time",
  "employment_statusSelf-employed",
  "employment_statusNot employed but looking for work",
  "employment_statusNot employed and not looking for work",
  "employment_statusPrefer not to answer",
  
  # Residence Area
  "residence_areaVillage (rural)",
  "residence_areaTrading Center (town)"
)

tidy_model1 <- tidy(logistic_followup_model1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    significance = ifelse(p.value < 0.05, "Significant", "Not Significant"),
    aOR = exp(estimate),
    aOR_lower = exp(lower),
    aOR_upper = exp(upper)
  )
tidy_model1 <- tidy_model1 %>%
  mutate(term = factor(term, levels = term_order)) %>%
  droplevels()

ggplot(tidy_model1, aes(x = aOR, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed") + # 在OR=1处参考线
  geom_errorbarh(aes(xmin = aOR_lower, xmax = aOR_upper, color = significance), height = 0.2) +
  geom_point(aes(color = significance), size = 2) +
  scale_x_continuous(trans = "log",  # aOR使用对数刻度
                     breaks = c(0.5, 1, 2, 5), 
                     limits = c(0.1, 10)) +
  labs(
    title = "Model 1",
    x = "Adjusted Odds Ratio (log scale)",
    y = "Predictors"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Significant" = "blue", "Not Significant" = "gray")) +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "bottom")

tidy_model2 <- tidy(logistic_followup_model2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    significance = ifelse(p.value < 0.05, "Significant", "Not Significant"),
    aOR = exp(estimate),
    aOR_lower = exp(lower),
    aOR_upper = exp(upper)
  )

tidy_model2 <- tidy_model2 %>%
  mutate(term = factor(term, levels = term_order)) %>%
  droplevels()

ggplot(tidy_model2, aes(x = aOR, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = aOR_lower, xmax = aOR_upper, color = significance), height = 0.2) +
  geom_point(aes(color = significance), size = 2) +
  scale_x_continuous(trans = "log",
                     breaks = c(0.5, 1, 2, 5), 
                     limits = c(0.1, 10)) +
  labs(
    title = "Model 2",
    x = "Adjusted Odds Ratio (log scale)",
    y = "Predictors"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Significant" = "blue", "Not Significant" = "gray")) +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "bottom")

# install.packages("broom")
# install.packages("dplyr")
# install.packages("kableExtra")

library(broom)
library(dplyr)
library(kableExtra)

term_order <- c(
  # Age
  "age_categoryQ1",
  "age_categoryQ2",
  "age_categoryQ3",
  "age_categoryQ4",
  
  # Gender
  "genderFemale",
  "genderMale",
  
  # Marital Status
  "marital_statusSingle",
  "marital_statusMarried",
  "marital_statusDivorced/Separated",
  "marital_statusWidowed",
  "marital_statusPrefer not to answer",
  
  # Religion
  "religionMuslim",
  "religionBaptist",
  "religionCatholic",
  "religionOther Christian",
  "religionOther",
  "religionPrefer not to answer",
  
  # Highest Education
  "highest_educationNo school/Did not complete primary",
  "highest_educationPrimary",
  "highest_educationSecondary",
  
  # Household Income
  "household_incomeAllowed me to save just a little",
  "household_incomeOnly just met my expenses",
  "household_incomeWas not sufficient, so needed to use savings to meet expenses",
  "household_incomeWas really not sufficient, so needed to borrow to meet expenses",
  "household_incomePrefer not to answer",
  
  # Employment Status
  "employment_statusEmployed full-time",
  "employment_statusEmployed part-time",
  "employment_statusSelf-employed",
  "employment_statusNot employed but looking for work",
  "employment_statusNot employed and not looking for work",
  "employment_statusPrefer not to answer",
  
  # Residence Area
  "residence_areaVillage (rural)",
  "residence_areaTrading Center (town)"
)

#### Model 1（Univariate Model）处理 ####
tidy_model1_raw <- tidy(logistic_followup_model1) %>%
  filter(term != "(Intercept)")

tidy_model1 <- tidy_model1_raw %>%
  mutate(
    OR = exp(estimate),
    OR_lower = exp(estimate - 1.96 * std.error),
    OR_upper = exp(estimate + 1.96 * std.error),
    OR_CI = paste0(round(OR, 2), " [", round(OR_lower, 2), ", ", round(OR_upper, 2), "]"),
    p = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  mutate(term = factor(term, levels = term_order)) %>%
  arrange(term) %>%
  select(term, OR_CI, p)

#### Model 2（Multivariate Model）处理 ####
tidy_model2_raw <- tidy(logistic_followup_model2) %>%
  filter(term != "(Intercept)")

tidy_model2 <- tidy_model2_raw %>%
  mutate(
    aOR = exp(estimate),
    aOR_lower = exp(estimate - 1.96 * std.error),
    aOR_upper = exp(estimate + 1.96 * std.error),
    aOR_CI = paste0(round(aOR, 2), " [", round(aOR_lower, 2), ", ", round(aOR_upper, 2), "]"),
    p2 = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  mutate(term = factor(term, levels = term_order)) %>%
  arrange(term) %>%
  select(term, aOR_CI, p2)

combined <- full_join(tidy_model1, tidy_model2, by = "term")

table_html <- combined %>%
  rename(" " = term,
         "OR [95% CI]" = OR_CI,
         "p (If_follow)" = p,
         "aOR [95% CI]" = aOR_CI,
         "p (If_follow2)" = p2) %>%
  kable("html", escape = FALSE, align = "lcccc",
        caption = "Association between factors and outcome") %>%
  add_header_above(c(" " = 1, "Model 1" = 2, "Model 2" = 2)) %>%
  kable_styling(full_width = FALSE, position = "center")

table_html

library(dplyr)
library(knitr)
library(kableExtra)

df_table <- data.frame(
  variable = c("if_follow", "if_follow2"),
  zeros = c(sum(completed_data_2022$if_follow == 0, na.rm = TRUE),
            sum(completed_data_2022$if_follow2 == 0, na.rm = TRUE)),
  ones = c(sum(completed_data_2022$if_follow == 1, na.rm = TRUE),
           sum(completed_data_2022$if_follow2 == 1, na.rm = TRUE))
)

df_table %>%
  kable(col.names = c("Variable", "Count(0)", "Count(1)"), caption = "Distribution of if_follow and if_follow2") %>%
  kable_styling(full_width = FALSE, position = "center")
