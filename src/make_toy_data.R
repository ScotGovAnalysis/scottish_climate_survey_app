
# ---- Packages ----
# install.packages("openxlsx")   # Uncomment if not installed
# install.packages("tibble")     # Uncomment if not installed
library(openxlsx)
library(tibble)

# ---- Parameters & Data generation ----
set.seed(42)

# Number of rows
n <- 300

# Grouping variable with 3 levels
grp <- sample(c("A", "B", "C"), size = n, replace = TRUE)

# @qa_1: similar proportions across groups (~50%)
qa1 <- rbinom(n, 1, 0.5)

# @qa_2: clear difference by group (A=0.8, B=0.75, C=0.2)
qa2 <- sapply(grp, function(g) {
  if (g == "A")      rbinom(1, 1, 0.8)
  else if (g == "B") rbinom(1, 1, 0.75)
  else               rbinom(1, 1, 0.2)
})

# Weights: random positive values
weight0 <- round(runif(n, 0.5, 2.0), 3)

# Combine into a Data frame (sheet "Data")
toy_df <- tibble(
  `@qa_1`   = qa1,
  `@qa_2`   = qa2,
  grp       = grp,
  `@weight0`= weight0
)

# ---- Build Variable Labels (sheet "Variable Labels") ----
# Per your spec: rows being the column names of the data in the Data tab (currently @qa_1, @qa_2)
# and the Label should be random text that is the SAME for both @qa_1 and @qa_2.

question_vars <- c("@qa_1", "@qa_2")
shared_label_text <- "Random question stem for both QA_1 and QA_2"

varlab_df <- tibble(
  Variable = names(toy_df),
  Label    = c(rep(shared_label_text, length(question_vars)), "group", "weight")
)

# ---- Build Value Labels (sheet "Value Labels") ----
# For each question variable, add rows for value 0 and 1 with random text labels.

label_for_0 <- "Random label for value 0"
label_for_1 <- "Random label for value 1"

vallab_df <- do.call(rbind, lapply(question_vars, function(v) {
  tibble(
    Variable = c(v, v),
    Value    = c(0, 1),
    Label    = c(paste(label_for_0, v, sep = "_"), paste(label_for_1, v, sep = "_"))
  )
}))

# ---- Write Excel workbook with three sheets ----
out_file <- "toy_grouped_workbook.xlsx"

# Using openxlsx::write.xlsx with a named list of data frames creates multiple sheets.
openxlsx::write.xlsx(
  x = list(
    "Data"             = toy_df,
    "Variable Labels"  = varlab_df,
    "Value Labels"     = vallab_df
  ),
  file = out_file,
  asTable = TRUE,     # optional: writes as Excel tables
  overwrite = TRUE
)

cat("Workbook written to:", normalizePath(out_file), "\n")

# ---- Optional: quick sanity checks printed to console ----
cat("\nQuick summary of proportions by group:\n")
print(
  aggregate(cbind(`@qa_1`, `@qa_2`) ~ grp, data = as.data.frame(toy_df), mean)
)
cat("\nVariable Labels:\n")
print(varlab_df)
cat("\nValue Labels (first rows):\n")
print(head(vallab_df))
