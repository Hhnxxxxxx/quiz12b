# Set seed for reproducibility
set.seed(853)

# Number of observations
n <- 1000

# Simulate independent variables
age_group <- sample(c('18-24', '25-34', '35-44', '45-54', '55+'), n, replace = TRUE)
gender <- sample(c('Male', 'Female', 'Other'), n, replace = TRUE)
income_group <- sample(c('Low', 'Medium', 'High'), n, replace = TRUE)
highest_education <- sample(c('High School', 'Bachelor', 'Master', 'Doctorate'), n, replace = TRUE)

# Encode categorical variables as numerical for simplicity in this simulation
age_group_num <- as.numeric(factor(age_group, levels = c('18-24', '25-34', '35-44', '45-54', '55+')))
gender_num <- as.numeric(factor(gender, levels = c('Male', 'Female', 'Other')))
income_group_num <- as.numeric(factor(income_group, levels = c('Low', 'Medium', 'High')))
education_num <- as.numeric(factor(highest_education, levels = c('High School', 'Bachelor', 'Master', 'Doctorate')))

# Arbitrary effects (these coefficients are just for the simulation purpose)
beta_age <- 0.05
beta_gender <- -0.1
beta_income <- 0.2
beta_education <- 0.15
intercept <- -0.5 # Base log-odds of supporting the party without considering effects

# Calculate log-odds
log_odds <- intercept + beta_age * age_group_num + beta_gender * gender_num + 
  beta_income * income_group_num + beta_education * education_num

# Transform log-odds to probability using logistic function
prob_support <- 1 / (1 + exp(-log_odds))

# Generate binary outcome based on probability
support_political_party <- ifelse(prob_support > 0.5, 1, 0)

# Create a data frame
simulated_data <- data.frame(age_group, gender, income_group, highest_education, support_political_party)

# View the first few rows of the dataset
head(simulated_data)



library(testthat)

# Assume simulated_data is your dataframe

# 1. Test the Number of Rows
test_that("Dataset has the correct number of observations", {
  expect_equal(nrow(simulated_data), 1000)
})

# 2. Test the Number of Columns
test_that("Dataset has the correct number of columns", {
  expect_equal(ncol(simulated_data), 5)
})

# 3. Test Column Types
test_that("Columns are of correct data types", {
  expect_true(is.factor(simulated_data$age_group))
  expect_true(is.factor(simulated_data$gender))
  expect_true(is.factor(simulated_data$income_group))
  expect_true(is.factor(simulated_data$highest_education))
  expect_true(is.numeric(simulated_data$support_political_party))
})

# 4. Test Unique Values for Categorical Variables
test_that("Categorical variables contain expected unique values", {
  expect_set_equal(unique(simulated_data$gender), c('Male', 'Female', 'Other'))
  expect_set_equal(unique(simulated_data$income_group), c('Low', 'Medium', 'High'))
  # Add similar checks for age_group and highest_education as per your simulation design
})

# 5. Test Range of Binary Outcome
test_that("Binary outcome contains only 0 and 1", {
  expect_true(all(simulated_data$support_political_party %in% c(0, 1)))
})

# 6. Test Missing Values
test_that("There are no missing values in the dataset", {
  expect_true(complete.cases(simulated_data))
})

# 7. Test Distribution of Gender (Assuming a reasonable distribution was aimed for)
test_that("Gender distribution is as expected", {
  # This test might need adjustment based on the specific expectations of your simulation
  gender_table <- table(simulated_data$gender)
  expect_true(all(gender_table >= 0)) # Placeholder test; adjust as necessary
})

# 8. Test Distribution of Age Groups
test_that("Age group distribution is reasonable", {
  # Similar to gender, adjust the test based on your simulation's specifics
  age_group_table <- table(simulated_data$age_group)
  expect_true(all(age_group_table > 0)) # Placeholder test; adjust as necessary
})

# 9. Test for Logical Consistency
test_that("Data is logically consistent", {
  # Example: Test if '18-24' age group does not have 'Doctorate'
  doctorate_young <- subset(simulated_data, age_group == '18-24' & highest_education == 'Doctorate')
  expect_equal(nrow(doctorate_young), 0)
})

# 10. Test Proportion of Support
test_that("Proportion of support is within expected bounds", {
  proportion_support <- mean(simulated_data$support_political_party)
  # Assuming a reasonable range is between 0.3 to 0.7 for this simulation
  expect_true(proportion_support >= 0.3 & proportion_support <= 0.7)
})

# Execute all tests
test_dir("path/to/your/tests")
