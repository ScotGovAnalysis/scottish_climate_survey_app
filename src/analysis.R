library(tidyverse)
library(readxl)
library(survey)

#################
# Function to perform weighted pairwise comparison
# Generalized function to perform weighted pairwise comparison
pairwise_comparison <- function(design, var1, var2, 
                                var1_level1, var1_level2, 
                                var2_ref_level) {
 
  # Create formula for subsetting
  subset_condition <- design$variables[[var1]] %in% c(var1_level1, var1_level2)
  
  # Subset to two levels of var1
  subset_data <- subset(design, subset_condition)
  
  # Drop unused factor levels
  subset_data$variables[[var1]] <- droplevels(subset_data$variables[[var1]])
  subset_data$variables[[var2]] <- droplevels(subset_data$variables[[var2]])
  
  # Create formula for chi-square test with backticks for special characters
  var1_safe <- paste0("`", var1, "`")
  var2_safe <- paste0("`", var2, "`")
  formula_str <- paste0("~", var1_safe, " + ", var2_safe)
  test_formula <- as.formula(formula_str)
  
  # Perform chi-square test
  test_result <- svychisq(test_formula, subset_data, statistic = "Chisq")
  
  # Calculate proportions for each group
  props <- prop.table(svytable(test_formula, subset_data), margin = 1)
  
  # Calculate difference in reference level proportion
  ref_level_diff <- props[var1_level1, var2_ref_level] - 
    props[var1_level2, var2_ref_level]
  
  return(list(
    test = test_result,
    props = props,
    ref_level_diff = ref_level_diff
  ))
}
##########

path <- "./data/Scottish Climate Survey - 2024 data + labels.xlsx"

data_tbl <- read_excel(path, sheet = "Data")
varlab   <- read_excel(path, sheet = "Variable Labels")
vallab   <- read_excel(path, sheet = "Value Labels")

# Normalise case
names(varlab) <- tolower(names(varlab))
names(vallab) <- tolower(names(vallab))

varlab <- varlab %>%
  transmute(variable = as.character(variable), label = as.character(label)) %>%
  distinct()

vallab <- vallab %>%
  transmute(variable = as.character(variable),
            value    = as.character(value),
            label    = as.character(label)) %>%
  distinct()

# Keep only labels present in Data
varlab <- varlab %>% filter(variable %in% names(data_tbl))
vallab <- vallab %>% filter(variable %in% names(data_tbl))

dat <- list(
  data   = tibble::as_tibble(data_tbl),
  varlab = varlab,
  vallab = vallab
)


dat$data <- dat$data %>% 
  mutate_all(~replace(., . == -99, NA)) 



#### Compare climate change knowledge across income groups
data_temp <- dat$data %>% 
  mutate_at(vars(qa1, incomeycombined), as.factor) %>% 
  mutate(
    incomeycombined = fct_collapse(
      as.factor(incomeycombined),
      Low = c("1","2"),
      Mid = c("3","4","5"),
      High = c("6","7","8")
    )
  )

# Create survey design object
design <- svydesign(ids = ~1, weights = data_temp$`@weight0`, data = data_temp)


design_sub <- subset(
  design,
  !is.na(qa1) & !is.na(incomeycombined) &
    incomeycombined %in% c("Low", "Mid", "High") &
    qa1 %in% seq(1, 4)
)

#  Drop unused factor levels
design_sub$variables$qa1 <- droplevels(design_sub$variables$qa1)
design_sub$variables$incomeycombined <- droplevels(design_sub$variables$incomeycombined)

weighted_table <- svytable(~qa1 + incomeycombined, design_sub)
print(round(weighted_table, 1))


weighted_props <- prop.table(svytable(~qa1 + incomeycombined, design_sub), 
                             margin = 2)
print(round(weighted_props, 3))

overall_test <- svychisq(~qa1 + incomeycombined, design_sub, statistic = "F")
print(overall_test)

#############################
###Compare climate change hope across income groups ###

data_temp <- dat$data %>% 
  mutate_at(vars(`@qa4_1`, incomeycombined), as.factor) %>% 
  mutate(
    incomeycombined = fct_collapse(
      as.factor(incomeycombined),
      Low = c("1","2"),
      Mid = c("3","4","5"),
      High = c("6","7","8")
    )
  )

# Create survey design object
design <- svydesign(ids = ~1, weights = data_temp$`@weight0`, data = data_temp)


design_sub <- subset(
  design,
  !is.na(`@qa4_1`) & !is.na(incomeycombined) &
    incomeycombined %in% c("Low", "Mid", "High")
)

#  Drop unused factor levels
design_sub$variables$`@qa4_1` <- droplevels(design_sub$variables$`@qa4_1`)
design_sub$variables$incomeycombined <- droplevels(design_sub$variables$incomeycombined)

weighted_table <- svytable(~`@qa4_1` + incomeycombined, design_sub)
print(round(weighted_table, 1))


weighted_props <- prop.table(svytable(~`@qa4_1` + incomeycombined, design_sub), 
                             margin = 2)
print(round(weighted_props, 3))

overall_test <- svychisq(~`@qa4_1` + incomeycombined, design_sub, statistic = "F")
print(overall_test)

## Post-hoc
# Perform all pairwise comparisons
comparisons <- list(
  "Low vs Mid" = pairwise_comparison(design_sub, var1 = "incomeycombined", 
                                     var2 = "@qa4_1", 
                                     var1_level1 = "Low",
                                     var1_level2 = "Mid", 
                                     var2_ref_level = "0"),
  "Low vs High" = pairwise_comparison(design_sub, var1 = "incomeycombined", 
                                      var2 = "@qa4_1", 
                                      var1_level1 = "Low", 
                                      var1_level2 = "High", 
                                      var2_ref_level = "0"),
  "Mid vs High" = pairwise_comparison(design_sub, var1 = "incomeycombined",
                                      var2 = "@qa4_1", 
                                      var1_level1 = "Mid",
                                      var1_level2 = "High", 
                                      var2_ref_level = "0")
)


# Extract p-values
p_values <- sapply(comparisons, function(x) x$test$p.value)
prop_diff <- map_dbl(comparisons, function(x) x$ref_level_diff)
n_comparisons <- length(p_values)

# Apply Bonferroni correction
adjusted_p <- p.adjust(p_values, method = "bonferroni")

# Display results
results_df <- data.frame(
  Comparison = names(p_values),
  Raw_p = round(p_values, 4),
  Bonferroni_p = round(adjusted_p, 4),
  Significant_05 = ifelse(adjusted_p < 0.05, "Yes", "No"),
  prop_diff = prop_diff
)

#############################
###Compare climate change powerlessness across income groups ###

data_temp <- dat$data %>% 
  mutate_at(vars(`@qa4_12`, incomeycombined), as.factor) %>% 
  mutate(
    incomeycombined = fct_collapse(
      as.factor(incomeycombined),
      Low = c("1","2"),
      Mid = c("3","4","5"),
      High = c("6","7","8")
    )
  )

# Create survey design object
design <- svydesign(ids = ~1, weights = data_temp$`@weight0`, data = data_temp)


design_sub <- subset(
  design,
  !is.na(`@qa4_12`) & !is.na(incomeycombined) &
    incomeycombined %in% c("Low", "Mid", "High")
)

#  Drop unused factor levels
design_sub$variables$`@qa4_12` <- droplevels(design_sub$variables$`@qa4_12`)
design_sub$variables$incomeycombined <- droplevels(design_sub$variables$incomeycombined)

weighted_table <- svytable(~`@qa4_12` + incomeycombined, design_sub)
print(round(weighted_table, 1))


weighted_props <- prop.table(svytable(~`@qa4_12` + incomeycombined, design_sub), 
                             margin = 2)
print(round(weighted_props, 3))

overall_test <- svychisq(~`@qa4_12` + incomeycombined, design_sub, statistic = "F")
print(overall_test)

## Post-hoc
# Perform all pairwise comparisons
comparisons <- list(
  "Low vs Mid" = pairwise_comparison(design_sub, var1 = "incomeycombined", 
                                     var2 = "@qa4_12", 
                                     var1_level1 = "Low",
                                     var1_level2 = "Mid", 
                                     var2_ref_level = "1"),
  "Low vs High" = pairwise_comparison(design_sub, var1 = "incomeycombined", 
                                      var2 = "@qa4_12", 
                                      var1_level1 = "Low", 
                                      var1_level2 = "High", 
                                      var2_ref_level = "1"),
  "Mid vs High" = pairwise_comparison(design_sub, var1 = "incomeycombined",
                                      var2 = "@qa4_12", 
                                      var1_level1 = "Mid",
                                      var1_level2 = "High", 
                                      var2_ref_level = "1")
)


# Extract p-values
p_values <- sapply(comparisons, function(x) x$test$p.value)
prop_diff <- map_dbl(comparisons, function(x) x$ref_level_diff)
n_comparisons <- length(p_values)

# Apply Bonferroni correction
adjusted_p <- p.adjust(p_values, method = "bonferroni")

# Display results
results_df <- data.frame(
  Comparison = names(p_values),
  Raw_p = round(p_values, 4),
  Bonferroni_p = round(adjusted_p, 4),
  Significant_05 = ifelse(adjusted_p < 0.05, "Yes", "No"),
  prop_diff = prop_diff
)
results_df

