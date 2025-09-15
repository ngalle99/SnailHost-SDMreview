# Calculating area inside the line for comparison to other studies
# Author: Nicholas A. Galle
# Affiliation: University of Notre Dame, Dept. of Biological Sciences, Rohr Lab
# Summary: This is code to calculate the area inside the line value at the 
# 50% quantile of studies (median) and 90% quantile of studies across all four 
# model aspects (response, predictor, model building, model evaluation), to make 
# comparisons to other fields. Results in manuscript are presented in Table 2.

# NOTE: Set working directory based on where "Supplement_3.csv" is saved
setwd("")
# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load in scores
scores1 <- read.csv("Supplement_3.csv")
Year <- scores1$Year
scores1 <- scores1 %>%
  mutate(across(everything(), ~ recode(.x, 
                                       "Deficient" = 0, 
                                       "Bronze" = 1, 
                                       "Silver" = 2, 
                                       "Gold" = 3)))
scores1 <- scores1[,1:15]
scores1 <- cbind(scores1, Year)
# do it for response variable
scores <- scores1[,1:5]

quantiles <- apply(scores, 2, function(x) {
  list(q50 = quantile(x, 0.50, na.rm = TRUE),
       q90 = quantile(x, 0.90, na.rm = TRUE))
})
# Define angles
n_aspects <- ncol(scores)
angles <- seq(0, 2*pi, length.out = n_aspects + 1)[-1]  # Avoid duplicate 0 and 2*pi
sdm_polar <- scores %>%
  mutate(Study = row_number()) %>%
  pivot_longer(-Study, names_to = "Aspect", values_to = "Score") %>%
  mutate(Theta = rep(angles, times = nrow(scores)))
# Plot radar chart 
ggplot(sdm_polar, aes(x = Theta, y = Score, group = Study)) +
  geom_polygon(aes(fill = as.factor(Study)), alpha = 0.2) +
  geom_line() +
  coord_polar() +
  theme_minimal()
compute_polar_area <- function(theta, r) {
  n <- length(theta)
  sum(r[-1] * r[-n] * sin(diff(theta))) / 2
}

# Calculate AIL
ail_scores <- sdm_polar %>%
  group_by(Study) %>%
  summarize(AIL = compute_polar_area(Theta, Score))
# Normalize AIL to percentage
ail_scores <- ail_scores %>%
  mutate(AIL_percent = (AIL / max(AIL)) * 100)
print(ail_scores)
ail.col <- ail_scores$AIL_percent
ail.sorted <- sort(ail.col)
resp.5 <- quantile(ail.sorted, probs=0.5)
resp.9 <- quantile(ail.sorted, probs=0.9)
# 50% Quantile for Response Variable
resp.5
# 90% Quantile for Response Variable
resp.9

# Now for predictor variable
scores <- scores1[,6:8]
quantiles <- apply(scores, 2, function(x) {
  list(q50 = quantile(x, 0.50, na.rm = TRUE),
       q90 = quantile(x, 0.90, na.rm = TRUE))
})
# Define angles 
n_aspects <- ncol(scores)
angles <- seq(0, 2*pi, length.out = n_aspects + 1)[-1]  # Avoid duplicate 0 and 2*pi
sdm_polar <- scores %>%
  mutate(Study = row_number()) %>%
  pivot_longer(-Study, names_to = "Aspect", values_to = "Score") %>%
  mutate(Theta = rep(angles, times = nrow(scores)))
# Plot radar chart
ggplot(sdm_polar, aes(x = Theta, y = Score, group = Study)) +
  geom_polygon(aes(fill = as.factor(Study)), alpha = 0.2) +
  geom_line() +
  coord_polar() +
  theme_minimal()
compute_polar_area <- function(theta, r) {
  n <- length(theta)
  sum(r[-1] * r[-n] * sin(diff(theta))) / 2
}
# Calculate AIL for each study
ail_scores <- sdm_polar %>%
  group_by(Study) %>%
  summarize(AIL = compute_polar_area(Theta, Score))
# Normalize AIL to percentage
ail_scores <- ail_scores %>%
  mutate(AIL_percent = (AIL / max(AIL)) * 100)
print(ail_scores)
ail.col <- ail_scores$AIL_percent
ail.sorted <- sort(ail.col)
preds.5 <- quantile(ail.sorted, probs=0.5)
preds.9 <- quantile(ail.sorted, probs=0.9)
# 50% Quantile for predictor variable
preds.5
# 90% quantile for predictor variable
preds.9


# Do it for model building 
scores <- scores1[,9:12]
quantiles <- apply(scores, 2, function(x) {
  list(q50 = quantile(x, 0.50, na.rm = TRUE),
       q90 = quantile(x, 0.90, na.rm = TRUE))
})
# Define angles
n_aspects <- ncol(scores)
angles <- seq(0, 2*pi, length.out = n_aspects + 1)[-1]  # Avoid duplicate 0 and 2*pi
sdm_polar <- scores %>%
  mutate(Study = row_number()) %>%
  pivot_longer(-Study, names_to = "Aspect", values_to = "Score") %>%
  mutate(Theta = rep(angles, times = nrow(scores)))
# Plot radar chart 
ggplot(sdm_polar, aes(x = Theta, y = Score, group = Study)) +
  geom_polygon(aes(fill = as.factor(Study)), alpha = 0.2) +
  geom_line() +
  coord_polar() +
  theme_minimal()
compute_polar_area <- function(theta, r) {
  n <- length(theta)
  sum(r[-1] * r[-n] * sin(diff(theta))) / 2
}
# Calculate AIL for each study
ail_scores <- sdm_polar %>%
  group_by(Study) %>%
  summarize(AIL = compute_polar_area(Theta, Score))
# Normalize AIL to percentage
ail_scores <- ail_scores %>%
  mutate(AIL_percent = (AIL / max(AIL)) * 100)
print(ail_scores)
ail.col <- ail_scores$AIL_percent
ail.sorted <- sort(ail.col)
build.5 <- quantile(ail.sorted, probs=0.5)
build.9 <- quantile(ail.sorted, probs=0.9)
# 50% quantile for model building
build.5
# 90% quantile for model building
build.9


# Finally for model evaluation 
scores <- scores1[,13:15]
quantiles <- apply(scores, 2, function(x) {
  list(q50 = quantile(x, 0.50, na.rm = TRUE),
       q90 = quantile(x, 0.90, na.rm = TRUE))
})

# Define angles 
n_aspects <- ncol(scores)
angles <- seq(0, 2*pi, length.out = n_aspects + 1)[-1]  # Avoid duplicate 0 and 2*pi
sdm_polar <- scores %>%
  mutate(Study = row_number()) %>%
  pivot_longer(-Study, names_to = "Aspect", values_to = "Score") %>%
  mutate(Theta = rep(angles, times = nrow(scores)))
# Plot radar chart
ggplot(sdm_polar, aes(x = Theta, y = Score, group = Study)) +
  geom_polygon(aes(fill = as.factor(Study)), alpha = 0.2) +
  geom_line() +
  coord_polar() +
  theme_minimal()
compute_polar_area <- function(theta, r) {
  n <- length(theta)
  sum(r[-1] * r[-n] * sin(diff(theta))) / 2
}
# Calculate AIL 
ail_scores <- sdm_polar %>%
  group_by(Study) %>%
  summarize(AIL = compute_polar_area(Theta, Score))
# Normalize AIL to percentage
ail_scores <- ail_scores %>%
  mutate(AIL_percent = (AIL / max(AIL)) * 100)
print(ail_scores)
ail.col <- ail_scores$AIL_percent
ail.sorted <- sort(ail.col)
eval.5 <- quantile(ail.sorted, probs=0.5)
eval.9 <- quantile(ail.sorted, probs=0.9)
# 50% quantile for model evaluation
eval.5
# 90% quantile for model evaluation
eval.9

# all together
resp.5
resp.9
preds.5
preds.9
build.5
build.9
eval.5
eval.9


