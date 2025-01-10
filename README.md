# Biostatistics Consulting Project "Loss to Follow-up"

## Project Overview

This repository contains the workflow, analyses, and outputs for a **biostatistics consulting project** aimed at analyzing longitudinal survey data collected in 2022 and 2024. The project is a collaborative consulting effort involving **data cleaning**, **imputation**, **statistical modeling**, and **exploratory analysis** to derive meaningful insights about survey follow-up patterns and participant characteristics.

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
- Initial data cleaning and codebook-level adjustments were performed using `missing_data_clean.ipynb`.

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

### 5. **Final Outputs**
- Summarized findings and actionable insights in the final report.
- Provided a fully reproducible workflow using R Markdown and R script.

---

## Repository Structure

### **Files**
1. **`analysis_report.pdf`**:
   - Final report summarizing the project’s methodology, findings, and recommendations.
   - Includes key visualizations, model results, and demographic insights.

2. **`analysis_script.R`**:
   - Full R script containing data cleaning, modeling, and visualization workflows.
   - Designed for reproducibility and efficient analysis.

3. **`biostats_consult_1101.Rmd`**:
   - R Markdown file for detailed step-by-step analysis.
   - Includes inline commentary and visualizations for exploratory and statistical analysis.

4. **`missing_data_clean.ipynb`**:
   - Jupyter Notebook for codebook-level data cleaning and initial standardization.
   - Includes variable recoding, addressing inconsistencies, and generating codebooks for both datasets.

### **Archived Files**
- All earlier drafts, raw datasets, and intermediate outputs are stored in the `documents` folder for reference and documentation purposes.

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
