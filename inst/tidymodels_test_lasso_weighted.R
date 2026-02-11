#This script tries to use weights to make LASSO more akin to chi-squared. However, it yields poor recall

library(tidymodels)
#library(vip)
library(shiny)
library(tidyverse)
library(themis)

# Source all function files
function_files <- list.files("./R/functions", pattern = "\\.R$", full.names = TRUE)
for (file in function_files) {
  source(file)
}

#############
#' Get cluster labels from hierarchical clustering at specified height
#'
#' @param hclust_obj An hclust object from hierarchical clustering
#' @param height Numeric. The height at which to cut the dendrogram
#' @param min_cor Numeric (optional). Alternative to height - specify minimum 
#'   correlation threshold (e.g., 0.6 for variables with |r| >= 0.6). 
#'   If provided, height is computed as 1 - min_cor
#'
#' @return A data frame with columns 'variable' and 'cluster'
#'
#' @examples
#' # Cut at height 0.4 (equivalent to min correlation of 0.6)
#' clusters <- get_cluster_labels(hclust_result, height = 0.4)
#' 
#' # Or specify minimum correlation directly
#' clusters <- get_cluster_labels(hclust_result, min_cor = 0.6)
#'
get_cluster_labels <- function(hclust_obj, height = NULL, min_cor = NULL) {
  
  # Check inputs
  if (is.null(height) && is.null(min_cor)) {
    stop("Must provide either 'height' or 'min_cor'")
  }
  
  if (!is.null(height) && !is.null(min_cor)) {
    stop("Provide only one of 'height' or 'min_cor', not both")
  }
  
  # Convert min_cor to height if provided
  if (!is.null(min_cor)) {
    if (min_cor < 0 || min_cor > 1) {
      stop("min_cor must be between 0 and 1")
    }
    height <- 1 - min_cor
  }
  
  # Cut tree at specified height
  cluster_assignments <- cutree(hclust_obj, h = height)
  
  # Convert to data frame
  cluster_df <- data.frame(
    variable = names(cluster_assignments),
    cluster = as.integer(cluster_assignments),
    stringsAsFactors = FALSE
  )
  
  # Sort by cluster then variable name for readability
  cluster_df <- cluster_df[order(cluster_df$cluster, cluster_df$variable), ]
  rownames(cluster_df) <- NULL
  
  return(cluster_df)
}
###############

path <- "./data/Scottish Climate Survey - 2024 data + labels.xlsx"


dat <- read_workbook(path, 6 * 1024^2)
data_tbl <- dat$data
varlab   <- dat$varlab
vallab   <-dat$vallab

data_tbl_processed <- data_tbl %>% 
  mutate(across(everything()), replace(., . == -99, NA)) %>% 
  filter(!is.na(qclimate1)) %>% 
  mutate(response = case_when(
    qclimate1 %in% as.character(2:4) ~ 2,
    qclimate1 %in% as.character(5:8) ~ 3, #collapse none of these, don't know, not stated
    TRUE ~ qclimate1
  )) %>% 
  mutate(case_wts = importance_weights(`@weight0`)) %>% 
  mutate(across(-c(contains("@weight"), case_wts), as.factor)) %>% 
  select(-`@weight0`, -`@weight1`       ) %>% 
  select(-qclimate1)

set.seed(123)
split <- initial_split(data_tbl_processed, strata = response)

train <- training(split)
test <- testing(split)

# Calculate class weights inversely proportional to class frequency
class_weights <- train %>%
  select(response) %>% 
  count(response) %>%
  mutate(weight = max(n) / n) %>%
  select(response, weight) %>% 
  deframe()

# rec_smote <- recipe(response ~ ., data = train) %>%
#   # 1) Impute missing categories
#   step_impute_mode(all_nominal_predictors()) %>%
#   
#   # 2) Remove single-level predictors *before* dummying
#   step_zv(all_nominal_predictors()) %>%
#   
#   # 3) Collapse rare levels to stabilize across folds (tune threshold if needed)
#   step_other(all_nominal_predictors(), threshold = 0.005) %>%
#   
#   
#   
#   # 4) One-hot / dummy encoding (reference encoding by default is fine for glmnet)
#   step_dummy(all_nominal_predictors()) %>%
#   
#   # 5) synthetic samples created in training folds only)
#   step_smote(response) %>% 
#   
#   # 6) Clean up any constant columns created after encoding
#   step_zv(all_predictors())

rec_weighted <- recipe(response ~ ., data = train) %>%
  #update_role(case_wts, new_role = "case_weights") %>%  # Or case_wts if using importance_weights
  step_impute_mode(all_nominal_predictors()) %>%
  step_zv(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.005) %>%
  step_dummy(all_nominal_predictors()) %>%
  # NO SMOTE for weighted inference
  step_zv(all_predictors())
    

mod_glm <-  parsnip::multinom_reg(
  penalty = tune::tune(),
  mixture = tune::tune()
) %>% parsnip::set_engine("glmnet", class.weights = class_weights)

param1 <- parameters(dials::penalty(range = c(-5, -2)), 
                              dials::mixture())


grid1 <- grid_regular(param1, levels = c(5, 5))
# lasso_grid <-  dials::grid_latin_hypercube(param_set_lasso, size = 50)


folds <- rsample::vfold_cv(v = 5,
                           strata = response, 
                           data = train)

wf <- 
  workflow() %>%
  add_model(mod_glm) %>%
  # add_case_weights(case_wts) %>% 
  add_recipe(rec_weighted)

ctrl <- control_grid(save_workflow = TRUE)



metrics <- yardstick::metric_set(accuracy, mn_log_loss, kap)

set.seed(456)
tune_glm <- tune_grid(wf, resamples = folds, grid = grid1, 
                        control = ctrl, metrics = metrics)

best_ll <- tune::select_best(tune_glm, metric = "mn_log_loss")
best_mixture <- best_ll$mixture


mod2 <- multinom_reg(penalty = tune(), mixture = best_mixture) %>%
  set_engine("glmnet", class.weights = class_weights)

param2 <- parameters(dials::penalty(range = c(-8, -1)))
grid2  <- grid_latin_hypercube(param2, size = 100) # fast now


wf2 <- 
  workflow() %>%
  add_model(mod2) %>%      # <- use the Stage-2 model here
  #dd_case_weights(case_wts) %>% 
  add_recipe(rec_weighted)

set.seed(456)
tune_glm2 <- tune_grid(
  wf2,                     # <- use wf2, not wf
  resamples = folds,
  grid = grid2,
  control = ctrl,
  metrics = metrics
)


best_ll2 <- tune::select_best(tune_glm2, metric = "mn_log_loss")

# tune_lasso <- tune_grid(wf, resamples = folds, grid = lasso_grid, 
#                         control = ctrl)

wf_final <- finalize_workflow(wf2, best_ll2)
fit_glm <- fit(wf_final, data = train)


prd <- predict(fit_glm, test)


precision_vec(test$response, prd$.pred_class, estimator = "macro_weighted")
recall_vec(test$response, prd$.pred_class, estimator = "macro_weighted")
accuracy_vec(test$response, prd$.pred_class, estimator = "macro_weighted")
bal_accuracy_vec(test$response, prd$.pred_class, estimator = "macro_weighted")


library(workflows)
library(broom)

# 1) Get the best penalty chosen during tuning (use the same metric you used for selection)
#best <- tune::select_best(tune_lasso, metric = "accuracy")
best_lambda <- best_ll$penalty

# 2) Pull the fitted parsnip model from the finalized workflow
fit_parsnip <- workflows::pull_workflow_fit(fit_glm)

# 3) Tidy the coefficients at that lambda.
#    return_zeros = TRUE keeps zeroed (lassoed) coefficients so you can see which were selected.
coef_tbl <- broom::tidy(
  fit_parsnip,
  penalty = best_lambda,
  return_zeros = TRUE
)

View(coef_tbl %>% 
       filter(estimate != 0))


# Confusion matrix
yardstick::conf_mat(test %>% mutate(.pred_class = prd$.pred_class), truth = response, estimate = .pred_class)

## ###########################
# Correlation and clustering 

prepped_recipe <- prep(rec_smote)
transformed_data <- bake(prepped_recipe, new_data = NULL)

# Now do clustering on the transformed predictors
predictor_cols <- transformed_data %>% select(-response)
cor_matrix <- cor(predictor_cols)
hclust_result <- hclust(as.dist(1 - abs(cor_matrix)), method = "complete")
clusters <- get_cluster_labels(hclust_result, min_cor = 0.3)

for (min_cor in c(0.3, 0.4, 0.5, 0.6, 0.7)) {
  clusters <- get_cluster_labels(hclust_result, min_cor = min_cor)
  n_clusters <- length(unique(clusters$cluster))
  n_singletons <- sum(table(clusters$cluster) == 1)
  cat(sprintf("min_cor = %.1f: %d clusters (%d singletons)\n", 
              min_cor, n_clusters, n_singletons))
}

plot(hclust_result, labels = FALSE, main = "Dendrogram")
abline(h = 0.5, col = "red", lty = 2)  # 50% correlation threshold
abline(h = 0.3, col = "blue", lty = 2) # 70% correlation threshold
