library(tidymodels)
library(vip)
library(shiny)
# Source all function files
function_files <- list.files("./R/functions", pattern = "\\.R$", full.names = TRUE)
for (file in function_files) {
  source(file)
}

path <- "./data/Scottish Climate Survey - 2024 data + labels.xlsx"


dat <- read_workbook(path, 6 * 1024^2)
data_tbl <- dat$data
varlab   <- dat$varlab
vallab   <-dat$vallab
# ###############
# data_tbl_processed <- data_tbl %>% 
#   select(-`@weight0`, -`@weight1`, 
#          -(starts_with("qc1") & !matches("qc1_1_actionscale_all"))) %>% 
#   mutate(across(everything()), replace(., . == -99, NA)) %>% 
#   filter(!is.na(qc1_1_actionscale_all)) %>% 
#   mutate(response = case_when(
#     qc1_1_actionscale_all %in% as.character(4:7) ~ 4, 
#     TRUE ~ qc1_1_actionscale_all
#   )) %>% 
#   mutate_all(as.factor) %>% 
#   select(-qc1_1_actionscale_all)
# 
# split <- initial_split(data_tbl_processed, strata = response)
# 
# train <- training(split)
# test <- testing(split)
# 
# 
# rec <- recipe(response ~ ., data = train) %>%
#   # 1) Impute missing categories
#   step_impute_mode(all_nominal_predictors()) %>%
#   
#   # 2) Remove single-level predictors *before* dummying
#   step_zv(all_nominal_predictors()) %>%
#   
#   # 3) Collapse rare levels to stabilize across folds (tune threshold if needed)
#   step_other(all_nominal_predictors(), threshold = 0.005) %>%
#   
#   # 4) One-hot / dummy encoding (reference encoding by default is fine for glmnet)
#   step_dummy(all_nominal_predictors()) %>%
#   
#   # 5) Clean up any constant columns created after encoding
#   step_zv(all_predictors())
# 
# 
# 
# 
# 
# mod_lasso <-  parsnip::multinom_reg(
#   penalty = tune::tune(),
#   mixture = 1
# ) %>% parsnip::set_engine("glmnet")
# 
# param_set_lasso <- parameters(dials::penalty())
# 
# lasso_grid <- dials::grid_regular(param_set_lasso)
# 
# 
# folds <- rsample::vfold_cv(v = 5,
#                            strata = response, 
#                            data = train)
# 
# wf <- 
#   workflow() %>%
#   add_model(mod_lasso) %>%
#   add_recipe(rec)
# 
# ctrl <- control_grid(save_workflow = TRUE)
# 
# set.seed(456)
# tune_lasso <- tune_grid(wf, resamples = folds, grid = lasso_grid, 
#                      control = ctrl)
# fit_lasso <- fit_best(tune_lasso)
# 
# prd <- predict(fit_lasso, test)
# #vip::vi(fit_rf)
# 
# 
# precision_vec(test$response, prd$.pred_class)
# recall_vec(test$response, prd$.pred_class)
# accuracy_vec(test$response, prd$.pred_class)
# bal_accuracy_vec(test$response, prd$.pred_class)

#> precision_vec(test$response, prd$.pred_class)
# [1] 0.3298142
# > recall_vec(test$response, prd$.pred_class)
# [1] 0.308258
# > accuracy_vec(test$response, prd$.pred_class)
# [1] 0.5982405
# > bal_accuracy_vec(test$response, prd$.pred_class)
# [1] 0.5574976
#########################
#########################data_tbl_processed <- data_tbl %>% 
data_tbl_processed <- data_tbl %>% 
  select(-`@weight0`, -`@weight1`       ) %>% 
  mutate(across(everything()), replace(., . == -99, NA)) %>% 
  filter(!is.na(qclimate1)) %>% 
  mutate(response = case_when(
    qclimate1 %in% as.character(2:4) ~ 2,
    qclimate1 %in% as.character(5:8) ~ 3, #collapse none of these, don't know, not stated
    TRUE ~ qclimate1
  )) %>% 
  mutate_all(as.factor) %>% 
  select(-qclimate1)

split <- initial_split(data_tbl_processed, strata = response)

train <- training(split)
test <- testing(split)


rec <- recipe(response ~ ., data = train) %>%
  # 1) Impute missing categories
  step_impute_mode(all_nominal_predictors()) %>%
  
  # 2) Remove single-level predictors *before* dummying
  step_zv(all_nominal_predictors()) %>%
  
  # 3) Collapse rare levels to stabilize across folds (tune threshold if needed)
  step_other(all_nominal_predictors(), threshold = 0.005) %>%
  
  # 4) One-hot / dummy encoding (reference encoding by default is fine for glmnet)
  step_dummy(all_nominal_predictors()) %>%
  
  # 5) Clean up any constant columns created after encoding
  step_zv(all_predictors())


mod_glm <-  parsnip::multinom_reg(
  penalty = tune::tune(),
  mixture = tune::tune(),
) %>% parsnip::set_engine("glmnet")

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
  add_recipe(rec)

ctrl <- control_grid(save_workflow = TRUE)



metrics <- yardstick::metric_set(accuracy, mn_log_loss, kap)

set.seed(456)
tune_glm <- tune_grid(wf, resamples = folds, grid = grid1, 
                        control = ctrl, metrics = metrics)

best_ll <- tune::select_best(tune_glm, metric = "mn_log_loss")
best_mixture <- best_ll$mixture


mod2 <- multinom_reg(penalty = tune(), mixture = best_mixture) %>%
  set_engine("glmnet")
param2 <- parameters(dials::penalty(range = c(-8, -1)))
grid2  <- grid_space_filling(param2, size = 100) # fast now


wf2 <- 
  workflow() %>%
  add_model(mod2) %>%      # <- use the Stage-2 model here
  add_recipe(rec)

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


