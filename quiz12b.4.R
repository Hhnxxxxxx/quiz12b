library(ggplot2)

# First, calculate the proportion of support within each age group
simulated_data$age_group <- factor(simulated_data$age_group, levels = c('18-24', '25-34', '35-44', '45-54', '55+'))
age_group_support <- aggregate(support_political_party ~ age_group, data = simulated_data, mean)

# Now, plot the proportion of support by age group using ggplot2
ggplot(age_group_support, aes(x = age_group, y = support_political_party, fill = age_group)) +
  geom_bar(stat = "identity") + 
  labs(title = "Proportion of Political Party Support by Age Group",
       x = "Age Group",
       y = "Proportion of Support",
       fill = "Age Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(label = round(support_political_party, 2)), vjust = -0.5)



library(rstanarm)

# Convert factors to character to avoid unused level issues in Bayesian analysis
simulated_data$age_group <- as.character(simulated_data$age_group)
simulated_data$gender <- as.character(simulated_data$gender)
simulated_data$income_group <- as.character(simulated_data$income_group)
simulated_data$highest_education <- as.character(simulated_data$highest_education)

# Fit a Bayesian logistic regression model
# The binary outcome is 'support_political_party'
# Predictors include 'age_group', 'gender', 'income_group', 'highest_education'
model <- stan_glm(support_political_party ~ age_group + gender + income_group + highest_education,
                  family = binomial(link = "logit"),
                  data = simulated_data,
                  chains = 4, # Number of Markov Chain Monte Carlo (MCMC) chains
                  iter = 2000, # Number of MCMC iterations per chain
                  seed = 12345) # For reproducibility
