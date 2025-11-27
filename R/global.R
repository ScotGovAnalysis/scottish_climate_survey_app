# global.R - Scottish Climate Survey dashboard
# Global settings and library loading

# Load required packages
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(DT)
library(scales)
library(survey)
library(tibble)

# Set survey options
options(survey.lonely.psu = "adjust")
options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB upload limit
options(shiny.fullstacktrace = TRUE)

# Source all function files
function_files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
for (file in function_files) {
  source(file)
}