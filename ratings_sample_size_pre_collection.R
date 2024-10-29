install.packages("pwr")
library(pwr)

# Set parameters
effect_size <- 0.25  # Medium effect size
alpha <- 0.05        # Significance level
power <- 0.80        # Desired power
num_groups <- 112    # Number of images

# Calculate sample size for ANOVA
sample_size_anova <- pwr.anova.test(k = num_groups, f = effect_size, sig.level = alpha, power = power)
print(sample_size_anova)


# Sample size for correlation
cor_effect_size <- 0.3  # Expected correlation
sample_size_corr <- pwr.r.test(r = cor_effect_size, sig.level = 0.05, power = 0.3)
print(sample_size_corr)


# Example of simulating and calculating sample size for mixed-effects models
library(Matrix)
library(lme4)
library(simr)

# Fit your model
model <- lmer(rating ~ image + (1|participant), data = your_data)

# Power analysis by simulation
powerSim(model, nsim = 100)  # Adjust nsim based on your needs