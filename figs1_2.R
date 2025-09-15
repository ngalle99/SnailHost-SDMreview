# Spatial/Temporal Trends and Extracted Methods
# Author: Nicholas A. Galle
# Affiliation: University of Notre Dame, Dept. of Biological Sciences, Rohr Lab
# Summary: This is a script to generate figures 1 (summary of extracted methodologies)
# and 2 (spatio-temporal publication trends).

# Load in libraries
library(tidyverse)
library(patchwork)
library(ggplot2)
#NOTE: Set directory based on where "Supplement_1.csv" is saved
setwd("")

# Load in extracted data
df <- read_csv("Supplement_1.csv")
valid_models <- c("ME", "RF", "GLM", "GAM", "GBM", "SVM", "MARS",
                  "LR", "ENV", "ANN", "XGB", "Kriging", 
                  "k-nearest neighbor", "CART")
model_counts <- df %>%
  filter(!is.na(Model)) %>%
  separate_rows(Model, sep = ";") %>%
  mutate(Model = str_trim(Model),
         Model = if_else(Model %in% valid_models, Model, "Other")) %>%
  count(Model, sort = TRUE)
full_model_names <- c(
  ME = "MaxEnt",
  RF = "Random Forest",
  GLM = "Generalized Linear Model",
  GAM = "Generalized Additive Model",
  GBM = "Gradient Boosting Machine",
  SVM = "Support Vector Machine",
  MARS = "Multivariate Adaptive Regression Splines",
  LR = "Logistic Regression",
  ENV = "Envelope Model",
  ANN = "Artificial Neural Network",
  XGB = "Extreme Gradient Boosting",
  Kriging = "Kriging",
  `k-nearest neighbor` = "k-Nearest Neighbor",
  CART = "Classification and Regression Trees",
  Other = "Other"
)

# Replace model codes with full names
model_counts <- model_counts %>%
  mutate(Model = recode(Model, !!!full_model_names))
model_plot<- ggplot(model_counts, aes(x = n, y = fct_reorder(Model, n), fill = n)) +
  geom_col(fill = "#FF7043") +
  scale_x_continuous(limits = c(0, 40)) +
  labs(x = "",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")
model_plot

# Define evaluation metrics
valid_evals <- c("AUC", "NO", "TSS", "ROC", "R2", "OR", "AIC")
eval_counts <- df %>%
  filter(!is.na(Evaluation)) %>%
  separate_rows(Evaluation, sep = ";") %>%
  mutate(
    Evaluation = str_trim(Evaluation),
    Evaluation = if_else(Evaluation %in% valid_evals, Evaluation, "Other")
  ) %>%
  count(Evaluation, sort = TRUE) %>%
  mutate(
    fill_label = if_else(Evaluation == "NO", "NO", Evaluation)  
  )
full_eval_names <- c(
  AUC = "Area Under the Curve",
  NO = "No Evaluation",
  TSS = "True Skill Statistic",
  Other = "Other",
  ROC = "Reciever Operating Curve",
  R2 = "Coefficient of Determination",
  OR = "Odds Ratio",
  AIC = "Akaike Information Criteria"
)
# Replace model codes with full names for plot
eval_counts <- eval_counts %>%
  mutate(Evaluation = recode(Evaluation, !!!full_eval_names))
eval_plot<-ggplot(eval_counts, aes(x = n, y = fct_reorder(Evaluation, n), fill = fill_label)) +
  geom_col(fill = "#4CAF50") +
  scale_x_continuous(limits = c(0, 40)) +
  labs(x = "Number of Studies",
    y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")
eval_plot
# Set up counts for type of predictor variables
pred_counts <- df %>%
  filter(!is.na(Predictors)) %>%
  separate_rows(Predictors, sep = ";") %>%
  count(Predictors, sort = TRUE)
pred_counts<-pred_counts[1:4,]
full_pred_names <- c(
 CLIM = "Climatic",
 ENV = "Environmental",
 WATER = "Water Characteristic",
 ANTH = "Anthropogenic"
)
# Replace model codes with full names
pred_counts <- pred_counts %>%
  mutate(Predictors = recode(Predictors, !!!full_pred_names))
pred_plot<- ggplot(pred_counts, aes(x = n, y = fct_reorder(Predictors, n), fill = n)) +
  geom_col(fill = "#3F51B5") +
  scale_x_continuous(limits = c(0, 40)) +
  labs(x = "",
       y = ""
  ) +
  theme_bw() +
  theme(legend.position = "none")
pred_plot

# Combine plots

combined_plot <- model_plot / eval_plot / pred_plot + plot_layout(heights = c(1,1,1))  # stacked vertically
print(combined_plot)
# Get max count for scale
max_count <- max(
  model_counts$n,
  eval_counts$n,
  pred_counts$n,
  if (exists("response_counts")) response_counts$n else 0
)
# Define common breaks and limit for x-axis
common_breaks <- pretty(c(0, max_count), n = 5)
common_limits <- range(common_breaks)

model_plot <- model_plot + scale_x_continuous(limits = common_limits-2, breaks = common_breaks)
eval_plot <- eval_plot + scale_x_continuous(limits = common_limits-2, breaks = common_breaks)
pred_plot <- pred_plot + scale_x_continuous(limits = common_limits-2, breaks = common_breaks)

# Final combined plot for figure 1
combined_plot <- model_plot / pred_plot / eval_plot + plot_layout(heights = c(1,1,1))
print(combined_plot)
# Save it
ggsave(
  filename = "combined_plot_new.png",
  plot = combined_plot,
  width = 7,     
  height = 8,    
  dpi = 300,      
  units = "in",    
  device = "png"   
)


# Load in file for year published
df <- read.csv("Supplement_1.csv")
year_counts <- df %>%
  count(Year)
# Fit Poisson regression
poisson_model <- glm(n ~ Year, data = year_counts, family = poisson())
summary(poisson_model)
# Slope is positive and p < 0.05, publications have significantly increased over time
year_plot<- ggplot(year_counts, aes(x = Year, y = n)) +
  geom_col(fill = "lightblue", colour="black") +  
  stat_smooth(method = "glm", 
              method.args = list(family = "poisson"), 
              se = TRUE, color = "red") +
  labs(x = "Publication Year",
       y = "Number of Studies") +
  theme_minimal(base_size = 14)+
  theme_bw()
year_plot
ggsave(plot=year_plot, filename="new_year_plot.png", dpi=300, device="png",
       width=9,height=5)


# Chi-sq to test evenness in publications across Africa, South America, and Asia
obs <- c(Africa = 40, Asia = 29, SouthAmerica = 27)
# Expected counts under equal distribution
exp <- rep(sum(obs)/length(obs), length(obs))
# Chi-square goodness-of-fit test (expected is 1/3 for each continent)
chisq.test(x = obs, p = rep(1/3, 3))
# Chi-sq=3.0625, df = 2, p-value=0.2163, thus distribution of studies
# is equal across Africa, South America, and Asia