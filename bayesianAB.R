rm(list=ls())
set.seed(681)

library(ggplot2)
library(bayesAB)

# Data: Number of conversions and total users for A and B
n_T <- 1000
x_T <- 70
n_C <- 900
x_C <- 50

# Prior parameters for the Beta distribution
alpha_T <- 1
beta_T <- 1
alpha_C <- 1
beta_C <- 1

# Posterior parameters
posterior_alpha_T <- alpha_T + x_T
posterior_beta_T <- beta_T + n_T - x_T
posterior_alpha_C <- alpha_C + x_C
posterior_beta_C <- beta_C + n_C - x_C

# Sample from the posterior distributions
posterior_obs_T <- rbeta(10000, posterior_alpha_T, posterior_beta_T)
posterior_obs_C <- rbeta(10000, posterior_alpha_C, posterior_beta_C)

# Estimate the probability that T is better than C
prob_T_better <- mean(posterior_obs_T > posterior_obs_C)
print(paste("Probability that T is better than C:", round(prob_T_better, digit=3)))

# Estimate the average treatment effect
treatment_effect <- mean(posterior_obs_T - posterior_obs_C)
print(paste("Average change in Y b/w T and C:", round(treatment_effect, digit=3)))

treatment_effect_limit <- (alpha_T + x_T)/(alpha_T + beta_T + n_T) - (alpha_C + x_C)/(alpha_C + beta_C + n_C)
print(treatment_effect_limit)

# Plot the posterior distributions
df <- data.frame(
  y = c(posterior_obs_T, posterior_obs_C),
  group = factor(rep(c("T", "C"), each = 10000))
)

ggplot(df, aes(x = y, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Posterior Distributions of Outcomes",
       x = "Y = 1", y = "Density") +
  theme_minimal()