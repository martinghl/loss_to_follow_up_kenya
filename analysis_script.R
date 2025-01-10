### Title: "biostats_consulting"

date <- Sys.Date()

### Load Libraries
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

### Load Datasets
data_2022 <- read_csv("S:\\biostats_consulting_lab\\cleaned_2022_survey_dta.csv")
data_2024 <- read_csv("S:\\biostats_consulting_lab\\cleaned_2024_survey_dta.csv")

head(data_2022)

### Clean Data: 2022 Dataset
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

### Fill Missing Values and Add Age
data_2022_cleaned <- data_2022_cleaned %>%
  mutate(across(c(religion, science_or_religion, science_contradict, religious), 
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

### Visualize Age Distribution
age_summary <- data_2022_cleaned %>%
  group_by(age_category) %>%
  summarize(
    Count = n(),
    Min_Age = min(age, na.rm = TRUE),
    Max_Age = max(age, na.rm = TRUE)
  )

print(age_summary)

# Bar plot for observations by age quantile
ggplot(age_summary, aes(x = age_category, y = Count, fill = age_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Observations by Age Quantile", x = "Age Quantile", y = "Count") +
  theme_minimal()

# Density plot to visualize age distribution
ggplot(data_2022_cleaned, aes(x = age, fill = age_category)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Quantile", x = "Age", y = "Density") +
  theme_minimal()

# Boxplot of age in each quantile
ggplot(data_2022_cleaned, aes(x = age_category, y = age, fill = age_category)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Quantile", x = "Age Quantile", y = "Age") +
  theme_minimal()

### Clean Data: 2024 Dataset
data_2024_cleaned <- data_2024 %>%
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
  select(
    dob, caseid, response_status, response_by, gender, highest_education, marital_status, parent_guardian, 
    employment_status, work_industry, people_speak_to_daily, 
    household_income, residence_area, religion, 
    specified_other_religion, call_status, survey_date
  ) %>%
  mutate(
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
    employment_status = case_when(
      employment_status %in% c("Self-employed (includes agribusiness)", "Peasant farmer") ~ "Self-employed",
      TRUE ~ employment_status
    ),
    highest_education = case_when(
      highest_education == "Prefer not to answer" ~ "Prefer not to answer [do not read aloud]",
      TRUE ~ highest_education
    )
  ) %>%
  mutate(highest_education = replace_na(highest_education, "Missing")) %>%
  mutate(marital_status = replace_na(marital_status, "Missing")) %>%
  mutate(parent_guardian = replace_na(parent_guardian, "Missing")) %>%
  mutate(work_industry = replace_na(work_industry, "Missing")) %>%
  mutate(people_speak_to_daily = replace_na(people_speak_to_daily, "Missing")) %>%
  mutate(household_income = replace_na(household_income, "Missing")) %>%
  mutate(residence_area = replace_na(residence_area, "Missing")) %>%
  mutate(employment_status = replace_na(employment_status, "Missing")) %>%
  mutate(religion = replace_na(religion, "Missing"))

head(data_2024_cleaned)

# Merge Datasets
merged_data <- full_join(data_2022_cleaned, data_2024_cleaned, by = "caseid", suffix = c("_2022", "_2024")) %>%
  mutate(lost = if_else(is.na(response_status) | response_status != "Answered the phone, correct respondent", 1, 0))

print(dim(merged_data))
