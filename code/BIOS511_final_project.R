#------------------------------------------------------------------------------
# Project Title:
# Author: Bryan Jacobs
# Date: 11-04-2025
#------------------------------------------------------------------------------


# Load Packages ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(GGally)
library(caret)
library(MASS)
library(broom)
library(DescTools)
library(performance)
library(MuMIn)
library(gt)


set.seed(123)



# Load Data -------------------------------------------------------------------
admissions <- read_csv("data/admissions.csv")
patient <- read_csv("data/patients.csv")


# Data Cleaning: Derive visits per calendar year. -----------------------------
# select relevant cols from each table
admissions_clean <- admissions |>
  dplyr::select(subject_id, admittime, insurance, race, marital_status)

patient_clean <- patient |>
  dplyr::select(subject_id, gender, anchor_age, anchor_year_group)

# join tables
patient_admissions <- admissions_clean |>
  left_join(patient_clean, by = "subject_id")

# calculate admission year
patient_admissions <- patient_admissions |>
  mutate(admit_year = lubridate::year(admittime))

# summarize by subject_id and year
visits_per_year <- patient_admissions |>
  group_by(subject_id, admit_year) |>
  summarize(
    num_visits = n(),
    insurance = first(na.omit(insurance)),
    race = first(na.omit(race)),
    marital_status = first(na.omit(marital_status)),
    gender = first(na.omit(gender)),
    anchor_age = first(anchor_age),
    anchor_year_group = first(anchor_year_group),
    .groups = "drop"
  )

# handle missing values, handle variable types
visits_per_year_clean <- visits_per_year |>
  drop_na(num_visits, insurance, gender, anchor_age, race) |>
  mutate(
    gender = factor(gender),
    race = factor(race),
    marital_status = factor(marital_status),
    insurance = factor(insurance, levels = c("Private", setdiff(unique(insurance), "Private"))),
    anchor_year_group = factor(anchor_year_group),
  )

# broad race categories
visits_per_year_clean <- visits_per_year_clean %>%
  mutate(race_simple = case_when(
    race %in% c("WHITE", "WHITE - RUSSIAN", "WHITE - OTHER EUROPEAN", 
                "WHITE - BRAZILIAN", "WHITE - EASTERN EUROPEAN") ~ "White",
    
    race %in% c("BLACK/AFRICAN AMERICAN", "BLACK/AFRICAN", 
                "BLACK/CAPE VERDEAN", "BLACK/CARIBBEAN ISLAND") ~ "Black",
    
    race %in% c("HISPANIC/LATINO - PUERTO RICAN", "HISPANIC/LATINO - DOMINICAN",
                "HISPANIC/LATINO - SALVADORAN", "HISPANIC/LATINO - GUATEMALAN",
                "HISPANIC OR LATINO", "HISPANIC/LATINO - MEXICAN",
                "HISPANIC/LATINO - CUBAN", "HISPANIC/LATINO - HONDURAN",
                "HISPANIC/LATINO - COLUMBIAN", "HISPANIC/LATINO - CENTRAL AMERICAN") ~ "Hispanic",
    
    race %in% c("ASIAN", "ASIAN - CHINESE", "ASIAN - SOUTH EAST ASIAN", 
                "ASIAN - KOREAN", "ASIAN - ASIAN INDIAN") ~ "Asian",
    
    race %in% c("AMERICAN INDIAN/ALASKA NATIVE") ~ "Native American",
    
    race %in% c("NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER") ~ "Pacific Islander",
    
    race %in% c("OTHER", "UNKNOWN", "UNABLE TO OBTAIN", "PATIENT DECLINED TO ANSWER", 
                "MULTIPLE RACE/ETHNICITY", "PORTUGUESE", "SOUTH AMERICAN") ~ "Other"
  ))

# set ref as White
visits_per_year_clean$race_simple <- factor(visits_per_year_clean$race_simple)
visits_per_year_clean$race_simple <- relevel(visits_per_year_clean$race_simple, ref = "White")


# Exploratory Data Analysis ---------------------------------------------------
### Summary Statistics
# summary of numeric variables
visits_per_year_clean |>
  summarize(
    mean_visits = mean(num_visits),
    median_visits = median(num_visits),
    min_visits = min(num_visits),
    max_visits = max(num_visits),
    mean_age = mean(anchor_age),
    median_age = median(anchor_age)
  ) |>
  gt() |>
  tab_header(title = "Visits Per Year Summary") |>
  fmt_number(columns = tidyselect::everything(), decimals = 3)

# summary of categorical variables
visits_per_year_clean |>
  group_by(insurance) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  gt() |>
  tab_header(title = "Patient Insurance Count") |>
  fmt_number(columns = tidyselect::everything(), decimals = 0)

### Distribution of Outcome
# histogram of number of visits
visits_per_year_clean |>
  ggplot(aes(x = num_visits)) +
  geom_histogram(binwidth = 1, bins = 10, fill = "skyblue", color = "black",
                 breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  theme_minimal() +
  labs(title = "Distribution of Hospital Visits per Year")

# mean vs variance check (if variance > mean, neg binomial; if =, Poisson)
visits_per_year_clean |>
  summarize(mean_visits = mean(num_visits), var_visits = var(num_visits)) |>
  gt() |>
  tab_header(title = "Mean vs Variance") 

### Categorical Predictors
# Boxplots of visits by insurance
visits_per_year_clean |>
  ggplot(aes(x = insurance, y = num_visits)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  ylim(0, 3) +
  labs(title = "Hospital Visits by Insurance Type") +
  coord_flip()

# Mean visits by category
visits_per_year_clean |>
  group_by(gender) |>
  summarize(mean_visits = mean(num_visits)) |>
  gt() |>
  tab_header(title = "Mean Visits by Gender")

# Correlation
cat_data <- visits_per_year_clean |>
  dplyr::select(dplyr::where(is.factor))

# Function to compute Cramér’s V for a pair of columns
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  DescTools::CramerV(tbl, bias.correct = TRUE)
}

# Compute all pairwise Cramér’s V values
cramer_matrix <- expand_grid(
  var1 = names(cat_data),
  var2 = names(cat_data)
) |>
  mutate(value = map2_dbl(var1, var2, ~ cramers_v(cat_data[[.x]], cat_data[[.y]]))) |>
  pivot_wider(names_from = var2, values_from = value)

cramer_matrix |>
  gt() |>
  tab_header(title = "Cramer Correlation Matrix") |>
  fmt_number(columns = tidyselect::everything(), decimals = 3)

# Modeling and Evaluation -----------------------------------------------------
# split data
train_index <- createDataPartition(visits_per_year_clean$num_visits, p = 0.7, list = FALSE)
train_data <- visits_per_year_clean[train_index, ]
test_data  <- visits_per_year_clean[-train_index, ]

### MODELS ###
# Poisson model
poisson_model <- glm(
  num_visits ~ insurance * race_simple + anchor_age + marital_status + gender,
  family = poisson(link = "log"),
  data = train_data
)

# Quasi-Poisson model
quasi_model <- glm(
  num_visits ~ insurance * race_simple + anchor_age + marital_status + gender,
  family = quasipoisson(link = "log"),
  data = train_data
)

# Negative Binomial model
nb_model <- MASS::glm.nb(
  num_visits ~ insurance * race_simple + anchor_age + marital_status + gender,
  data = train_data
)


### EVALUATION ###
# Compare AIC (only valid for Poisson and Negative Binomial)
AIC(poisson_model, nb_model) |>
  gt(rownames_to_stub = TRUE) |>
  tab_header(title = "Alkaline Information Criterion")
  

# Pseudo R² (McFadden)
pseudo_r <- rbind(
  poisson  = MuMIn::r.squaredGLMM(poisson_model),
  quasi    = MuMIn::r.squaredGLMM(quasi_model),
  negbin   = MuMIn::r.squaredGLMM(nb_model)
)

# Check overdispersion
performance::check_overdispersion(poisson_model)
performance::check_overdispersion(quasi_model)
performance::check_overdispersion(nb_model)

# Root Mean Squared Error (using fitted vs observed)
get_rmse <- function(model) {
  sqrt(mean((model$y - fitted(model))^2))
}

data.frame(
  Model = c("Poisson", "Quasi-Poisson", "NegBinomial"),
  RMSE = c(
    get_rmse(poisson_model),
    get_rmse(quasi_model),
    get_rmse(nb_model)
  )
) |>
  gt() |>
  tab_header(title = "Root Mean Squared Error")

# Extract actual data used in each model
poisson_df <- model.frame(poisson_model) |>
  mutate(predicted = fitted(poisson_model),
         model = "Poisson")

quasi_df <- model.frame(quasi_model) |>
  mutate(predicted = fitted(quasi_model),
         model = "Quasi-Poisson")

nb_df <- model.frame(nb_model) |>
  mutate(predicted = fitted(nb_model),
         model = "NegBinomial")

# Combine safely
predictions <- bind_rows(poisson_df, quasi_df, nb_df) |>
  rename(actual = num_visits)

# Visualize
ggplot(predictions, aes(x = actual, y = predicted, color = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Counts by Model",
    x = "Actual num_visits",
    y = "Predicted num_visits"
  ) +
  theme_minimal()


# Interpretation --------------------------------------------------------------
# Function to tidy and filter insurance & interactions
tidy_insurance <- function(model, model_name) {
  coefs <- broom::tidy(model)
  
  # If 'term' column doesn't exist, create it from rownames
  if (!"term" %in% colnames(coefs)) {
    coefs$term <- rownames(coefs)
  }
  
  coefs |>
    dplyr::filter(stringr::str_detect(term, "insurance")) %>%   # keep insurance main effects and interactions
    dplyr::mutate(
      model = model_name,
      percent_change = round((exp(estimate) - 1) * 100, 1)  # % change interpretation
    ) |>
    dplyr::select(model, term, estimate, std.error, statistic, p.value, percent_change)
}

# Create tables for each model
poisson_table <- tidy_insurance(poisson_model, "Poisson")
quasi_table   <- tidy_insurance(quasi_model, "Quasi-Poisson")
nb_table      <- tidy_insurance(nb_model, "NegBinomial")

# Convert each to a gt table for presentation
poisson_gt <- poisson_table |>
  gt() |>
  tab_header(title = "Poisson Model: Insurance Effects") |>
  fmt_number(columns = tidyselect::everything(), decimals = 3)

quasi_gt <- quasi_table |>
  gt() |>
  tab_header(title = "Quasi-Poisson Model: Insurance Effects") |>
  fmt_number(columns = tidyselect::everything(), decimals = 3)

nb_gt <- nb_table |>
  gt() |>
  tab_header(title = "Negative Binomial Model: Insurance Effects") |>
  fmt_number(columns = tidyselect::everything(), decimals = 3)

# Display the tables
poisson_gt
quasi_gt
nb_gt
