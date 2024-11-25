
# README: Biostatistics Consulting Project

## Project Overview

This repository contains the workflow, analyses, and outputs for a **biostatistics consulting project** aimed at analyzing longitudinal survey data collected in 2022 and 2024. The project is not a tool or application but a collaborative consulting effort involving **data cleaning**, **imputation**, **statistical modeling**, and **exploratory analysis** to derive meaningful insights about survey follow-up patterns and participant characteristics.

### Objectives
The project seeks to:
1. **Analyze survey retention and loss-to-follow-up trends** between 2022 and 2024.
2. Investigate predictors of follow-up and non-response using **logistic regression models**.
3. Explore differences in demographic, socioeconomic, and behavioral characteristics between lost and followed groups.
4. Provide actionable insights and reproducible workflows for the client to inform future survey designs and retention strategies.

---

## Key Components

### 1. **Data Cleaning**
- Standardized and harmonized datasets from the 2022 and 2024 surveys.
- Converted categorical variables to appropriate formats.
- Merged datasets based on unique participant identifiers (`caseid`) to track changes over time.
- Addressed missing values using multiple imputation techniques.

### 2. **Exploratory Data Analysis (EDA)**
- Performed summary statistics for key variables.
- Visualized distributions of variables (e.g., age quantiles, categorical predictors).
- Assessed missingness and inconsistencies across datasets.

### 3. **Statistical Modeling**
- Developed **logistic regression models** to identify predictors of follow-up (e.g., gender, employment status, age quantiles).
- Compared two models with varying follow-up criteria:
  - **Model 1**: Stricter follow-up definition (completed survey only).
  - **Model 2**: Broader follow-up definition (includes partial completions, reschedules, and refusals).
- Evaluated model performance using metrics like ROC curves and McFadden’s R².

### 4. **Visualization**
- Plotted density distributions, bar charts, boxplots, and pie charts for demographic and survey responses.
- Performed dimensionality reduction techniques like **PCA** and **t-SNE** to visualize group separations (e.g., lost vs. followed participants).

### 5. **Codebook Generation**
- Created descriptive mappings of variables and categorical codings to enhance interpretability.
- Included metadata for the 2022 and 2024 datasets based on provided codebooks.

---

## Repository Structure

### **Scripts**
- **`missing_data_clean.ipynb`**: Jupyter Notebook for initial data cleaning and generating codebooks.
- **`biostats_consult_1124.Rmd`**: R Markdown file containing the full statistical analysis, modeling, and visualizations.

### **Data Files**
> *(Data files are not included in this repository due to confidentiality. Please ensure access permissions before using the scripts.)*
- `2022SurveyedPeople.dta`: Original 2022 survey dataset.
- `2024SurveyedPeople.dta`: Original 2024 survey dataset.
- Cleaned datasets:
  - `cleaned_2022_survey_dta.csv`
  - `cleaned_2024_survey_dta.csv`

### **Outputs**
- Model summaries for **logistic regression**.
- Visualizations for follow-up patterns and demographic distributions.
- Codebooks for 2022 and 2024 datasets.

---

## Usage Instructions

1. **Data Cleaning**:
   Run `missing_data_clean.ipynb` to clean and harmonize the raw datasets. This step includes:
   - Standardizing date formats.
   - Recoding categorical variables.
   - Generating codebooks for metadata.

2. **Data Analysis**:
   Execute `biostats_consult_1124.Rmd` in RStudio to perform:
   - Exploratory Data Analysis (EDA).
   - Logistic regression modeling.
   - Visualizations for insights into predictors and group differences.

---

## Key Findings

- **Age and gender** were significant predictors of follow-up in both models.
- **Employment status** showed stronger associations in the broader follow-up criteria (Model 2).
- Significant differences were observed in educational attainment and household income between lost and followed participants.
- Data visualizations revealed clustering patterns that highlight group differences, aiding in hypothesis generation for retention strategies.
---

## Notes for Consulting

- The analyses are tailored to the client's requirements and survey structure. Additional adjustments may be needed if survey design or objectives evolve.
- This repository is designed for reproducibility and collaboration. Please ensure that all data files are securely stored and accessed only by authorized personnel.

---

## Acknowledgments

This consulting project was conducted collaboratively under the supervision of **Dr. Corrina Moucheraud** and **Dr. Alex Dahlen**. Special thanks to the survey team for providing comprehensive datasets and metadata.

For inquiries or feedback, please contact the primary consultant at [gl1768@nyu.edu].
