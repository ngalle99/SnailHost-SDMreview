# Temporal trends in adherence to best practices
# Author: Nicholas A. Galle
# Affiliation: University of Notre Dame, Dept. of Biological Sciences, Rohr Lab
# Summary: This is an R script to quantify temporal trends in adherence
# to best modeling practices using Bayesian ordinal regression. Priors and settings
# based on those used in Barker & MacIsaac 2022 review on mosquito SDMs.

#NOTE: Change working directory to where the file "Supplement_3.csv" is saved
setwd("")

# Load in libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstanarm)
library(tidyverse)
library(broom.mixed) 
library(nnet)
library(ordinal)

# Load in study scores
scores1 <- read.csv("Supplement_3.csv")
year <- scores1$Year
scores1 <- scores1 %>%
  mutate(across(everything(), ~ recode(.x, 
                                       "Deficient" = 0, 
                                       "Bronze" = 1, 
                                       "Silver" = 2, 
                                       "Gold" = 3)))
scores1 <- scores1[,1:15]
scores1 <- cbind(scores1, year)
df_long <- scores1 %>%
  pivot_longer(cols = -year, names_to = "issue", values_to = "score")
# Calculate quantiles
quantiles <- df_long %>%
  group_by(issue) %>%
  summarise(q50 = quantile(score, 0.5, na.rm = TRUE),
            q90 = quantile(score, 0.9, na.rm = TRUE))

# Calculate area in polar coords
calculate_area <- function(scores) {
  n <- length(scores)
  theta <- seq(0, 2*pi, length.out = n + 1)[-1]
  x <- scores * cos(theta)
  y <- scores * sin(theta)
  area <- 0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
  max_area <- 0.5 * sum((3 * cos(theta)) * c(3 * sin(theta[-1]), 3 * sin(theta[1])) -
                          (3 * sin(theta)) * c(3 * cos(theta[-1]), 3 * cos(theta[1])))
  return(area / max_area * 100) # Normalize
}
# Apply to each quantile line
area_50 <- df_long %>%
  group_by(issue) %>%
  summarise(median_score = quantile(score, 0.5, na.rm = TRUE)) %>%
  pull(median_score) %>%
  calculate_area()
area_90 <- df_long %>%
  group_by(issue) %>%
  summarise(high_score = quantile(score, 0.9, na.rm = TRUE)) %>%
  pull(high_score) %>%
  calculate_area()
df_ord <- df_long %>%
  mutate(issue = factor(issue),
         score = ordered(score, levels = 0:3))  # ordinal factor

# Set seed for replication
set.seed(123)
model <- stan_polr(score ~ year * issue,
                   data = df_ord,
                   prior = R2(0.1),
                   chains = 4,
                   iter = 5000,
                   warmup = 1000,
                   seed = 123)
summary(model)

pred_data <- expand.grid(
  year = sort(unique(df_ord$year)),
  issue = levels(df_ord$issue)
)

# Get linear predictors
eta_draws <- posterior_linpred(model, newdata = pred_data, transform = FALSE)
# Get thresholds
cutpoints <- model$zeta  
inv_cumulative_logit <- function(eta, cutpoints) {
  probs <- matrix(NA, nrow = length(eta), ncol = length(cutpoints) + 1)
  for (i in 1:length(eta)) {
    p <- plogis(cutpoints - eta[i])
    probs[i, 1] <- p[1]
    probs[i, length(cutpoints) + 1] <- 1 - p[length(cutpoints)]
    if (length(cutpoints) > 1) {
      for (k in 2:length(cutpoints)) {
        probs[i, k] <- p[k] - p[k - 1]
      }
    }
  }
  return(probs)
}
draws <- dim(eta_draws)[1]
n_obs <- dim(eta_draws)[2]
categories <- c("Deficient", "Bronze", "Silver", "Gold")
all_probs <- purrr::map_dfr(1:draws, function(d) {
  probs <- inv_cumulative_logit(eta_draws[d, ], cutpoints)
  probs_df <- as_tibble(probs)
  names(probs_df) <- categories
  probs_df$draw <- d
  probs_df$row <- 1:n_obs
  probs_df
})
pred_data$row <- 1:nrow(pred_data)
pred_long <- all_probs %>%
  pivot_longer(cols = all_of(categories), names_to = "score", values_to = "prob") %>%
  left_join(pred_data, by = "row")

#Plot
summary_df <- pred_long %>%
  group_by(year, issue, score) %>%
  summarise(
    mean_prob = mean(prob),
    lower = quantile(prob, 0.1),
    upper = quantile(prob, 0.9),
    .groups = "drop"
  )
ggplot(summary_df, aes(x = year, y = mean_prob, color = issue)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = issue), alpha = 0.15, color = NA) +
  facet_wrap(~ score, scales = "free_y") +
  labs(
    title = "Temporal Trends in SDM Quality Scores",
    x = "Year", y = "Predicted Probability",
    color = "Issue", fill = "Issue"
  ) +
  theme_minimal()

# Get posterior samples 
posterior <- as.data.frame(model)

# Get interaction terms with year 
year_coefs <- posterior %>%
  select(starts_with("year:issue")) %>%
  pivot_longer(everything(), names_to = "term", values_to = "estimate")
year_coefs <- posterior %>%
  select(matches("year.*issue")) %>%
  pivot_longer(everything(), names_to = "term", values_to = "estimate")
posterior <- as.data.frame(model)

# Get levels of issues in the original model
issues <- levels(df_ord$issue)
reference_issue <- issues[1]

# Get interaction slopes
all_slopes <- purrr::map_dfr(issues, function(iss) {
  if (iss == reference_issue) {
    est <- posterior[["year"]]
  } else {
    interaction_col <- paste0("year:issue", iss)
    
    if (!interaction_col %in% names(posterior)) {
      stop(paste("Interaction term", interaction_col, "not found in posterior"))
    }
    est <- posterior[["year"]] + posterior[[interaction_col]]
  }
  
  tibble(
    issue = iss,
    mean  = mean(est),
    lower = quantile(est, 0.025),
    upper = quantile(est, 0.975)
  )
})
issue_labels <- c(
  X1a = "Sampling",
  X1b = "Taxa ID",
  X1c = "Spatial Accuracy",
  X1d = "Environmental Extent",
  X1e = "Geographic Extent",
  X2a = "Choice of Variables",
  X2b = "Spat./Temp. Resolution",
  X2c = "Predictor Uncertainty",
  X3a = "Model Complexity",
  X3b = "Bias and Noise",
  X3c = "Collinearity",
  X3d = "Model/Param. Uncertainty",
  X4a = "Model Assumptions",
  X4b = "Model Outputs",
  X4c = "Model Performance"
)
all_slopes <- all_slopes %>%
  mutate(
    issue_label = issue_labels[issue],
    issue_label = factor(issue_label, levels = rev(unname(issue_labels)))  # Reverse for top-to-bottom
  )
ggplot(all_slopes, aes(x = mean, y = issue_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "steelblue", alpha = 0.8) +
  labs(
    x = "Temporal Trend (Bayesian coefficient for Year)",
    y = "Modeling Issue",
    title = "Temporal Trends in Best-Practice Standards") +
  theme_minimal(base_size = 13)
# Fix up plot for colors and such
aspects <- c("Response","Response","Response","Response","Response","Predictor","Predictor","Predictor","Model Building","Model Building","Model Building","Model Building","Model Evaluation","Model Evaluation","Model Evaluation")
all_slopes$aspect <- aspects
# THIS IF FIGURE 4 IN THE MANUSCRIPT
ggplot(all_slopes, aes(x = mean, y = issue_label, color = aspect)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.4, alpha = 0.95, linewidth=1) +
  labs(
    x = "Temporal Trend",
    y = "Modeling Issue",
    color = "Aspect") +
  scale_color_manual(
    values = c(
      "Response" = "forestgreen",
      "Predictor" = "orange",
      "Model Building" = "dodgerblue",
      "Model Evaluation" = "magenta"
    ),
    breaks = c("Response", "Predictor", "Model Building", "Model Evaluation")  
  ) +
  theme_bw(base_size = 13)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
