# tests/testthat.R
# Test runner for testthat

library(testthat)

# Source all function files (same as in global.R)
function_files <- list.files("../../functions", pattern = "\\.R$", full.names = TRUE)
for (file in function_files) {
  source(file)
}

# Run all tests
test_check("SCS_dashboard")