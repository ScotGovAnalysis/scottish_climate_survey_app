# functions/data_io.R
# Functions for reading and validating workbook data

#' Read and validate Excel workbook
#' @param path Path to Excel file
#' @param size File size in bytes
#' @return List containing data, varlab, vallab, and sheets
read_workbook <- function(path, size) {
  # Validate file size
  if (size > 10 * 1024^2) {
    notify("File is larger than 10 MB.", "error")
    validate(need(FALSE, "File is larger than 10 MB."))
  }
  
  # Read sheet names
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
    notify(paste("Error reading sheets:", e$message), "error", duration = NULL)
    NULL
  })
  validate(need(!is.null(sheets), "Could not read the workbook. Is it a valid .xlsx file?"))
  
  # Check required sheets
  need_sheets <- c("Data", "Variable Labels", "Value Labels")
  if (!all(need_sheets %in% sheets)) {
    msg <- paste0("Workbook must contain sheets: ", paste(need_sheets, collapse = ", "))
    notify(msg, "error", duration = NULL)
    validate(need(FALSE, msg))
  }
  
  # Read sheets
  data_tbl <- readxl::read_excel(path, sheet = "Data")
  varlab   <- readxl::read_excel(path, sheet = "Variable Labels")
  vallab   <- readxl::read_excel(path, sheet = "Value Labels")
  
  # Normalize column names
  names(varlab) <- tolower(names(varlab))
  names(vallab) <- tolower(names(vallab))
  
  # Validate columns
  validate(
    need(all(c("variable", "label") %in% names(varlab)),
         "'Variable Labels' must have columns: Variable, Label."),
    need(all(c("variable", "value", "label") %in% names(vallab)),
         "'Value Labels' must have columns: Variable, Value, Label.")
  )
  
  # Clean up label tables
  varlab <- varlab %>%
    dplyr::transmute(variable = as.character(variable), label = as.character(label)) %>%
    dplyr::distinct() %>%
    dplyr::filter(variable %in% names(data_tbl))
  
  vallab <- vallab %>%
    dplyr::transmute(
      variable = as.character(variable),
      value    = as.character(value),
      label    = as.character(label)
    ) %>%
    dplyr::distinct() %>%
    dplyr::filter(variable %in% names(data_tbl))
  
  list(
    data   = tibble::as_tibble(data_tbl),
    varlab = varlab,
    vallab = vallab,
    sheets = sheets
  )
}

#' Classify variables into questions, groupings, and weights
#' @param data Data frame
#' @param varlab Variable labels data frame
#' @return List with q, g, and w data frames
classify_variables <- function(data, varlab) {
  vl <- varlab %>% dplyr::filter(variable %in% names(data))
  
  # Questions: start with @q or q
  q_vars <- vl %>%
    dplyr::filter(stringr::str_detect(variable, "^@?q")) %>%
    dplyr::mutate(choice_lab = dplyr::if_else(is.na(label) | label == "", variable, label)) %>%
    dplyr::arrange(choice_lab)
  
  # Groupings: exclude weighting variables
  g_vars <- vl %>%
    dplyr::filter(!stringr::str_detect(variable, "^@weight")) %>%
    dplyr::mutate(choice_lab = dplyr::if_else(is.na(label) | label == "", variable, label)) %>%
    dplyr::arrange(choice_lab)
  
  # Weights: start with @weight
  w_vars <- vl %>%
    dplyr::filter(stringr::str_detect(variable, "^@weight")) %>%
    dplyr::mutate(choice_lab = dplyr::if_else(
      is.na(label) | label == "", 
      variable,
      paste0(label, " (", variable, ")")
    )) %>%
    dplyr::arrange(choice_lab)
  
  list(q = q_vars, g = g_vars, w = w_vars)
}