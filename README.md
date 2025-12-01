# Analysis of Government Insurance Programs and Their Role in Equitable Healthcare Access
*Final project for [BIOS 511: Healthcare Data Science](https://infosci.arizona.edu/course/bios-511-healthcare-data-science) — University of Arizona*
  
  
## Project Intro/Objective
This analysis examines the effectiveness of government insurance programs' ability to reduce the disparity in acess to care based on demogrpahic groups in the United States. The goal is to understand if government insurance programs, when used by minority groups, are associated with an increased usage of the American healthcare system compared to white-identifying groups with private insurance.

  
## Dataset
The dataset used for this project is part of the MIMIC-IV collection of EHR data. Due the the credentialing process to access the data, including completing human subjects protection and HIPAA training as well as signing a Data Use Agreement, I am unable to share the dataset here.
  
  
## Authors
**Bryan Jacobs**    
  
  
## Languages/Packages:
* R
  * tidyverse
  * lubridate
  * GGally
  * caret
  * MASS
  * broom
  * DescTools
  * performance
  * MuMIn
  * gt
  
  
## Software & Platforms
* RStudio


## Models & Methods
* Poisson Regression
* Negative Binomial Regression
* Quasi-Poisson Regression
* Statistical Analysis
  
  
## Repository Structure
- **`code/`**: Contains `.R` file for data preparation, EDA, regression models and associated visualizations.
- **`deliverables/`**: Contains `.pdf` files of final report and presentation.
- **`README.md`**
  
  
## How To Run
#### For Simple Viewing
1. Download and open desired `.pdf` file from the `deliverables/` folder.

#### To Run Script
1. ACCESS TO MIMIC-IV DATA IS REQUIRED TO RUN THIS SCRIPT.
2. Download admissions.csv and patients.csv from MIMIC-IV.
3. Adjust file paths in script to align with location of your admissions.csv and patients.csv.
4. Run the script.
