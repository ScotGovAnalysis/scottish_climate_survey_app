# functions/analysis.R
# Functions for preparing and analyzing survey data

#' Prepare analysis data
#' @param data Raw data frame
#' @param varlab Variable labels
#' @param vallab Value labels
#' @param var Question variable
#' @param grp1 Primary grouping variable
#' @param grp2 Secondary grouping variable
#' @param filter_var Variable to be used for filtering survey
#' @param wgt Weighting variable
#' @param drop_empty Drop empty groups flag
#' @return List with df and question_text
prepare_analysis_data <- function(data, varlab, vallab,
                                  var, grp1, grp2, filter_var,  wgt,
                                  drop_empty) {
 browser()
   # Select relevant columns
  sel <- unique(c(var, grp1,
                  if (nzchar(grp2)) grp2 else NULL,
                  if (nzchar(wgt)) wgt else NULL, 
                  if (nzchar(filter_var)) filter_var else NULL))
  sel <- sel[sel %in% names(data)]
  
  validate(need(
    all(c(var, grp1) %in% sel),
    "Selected variables must exist in the Data sheet."
  ))
  
  # Prepare data
  df <- data %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(dplyr::all_of(c("id", sel)))
  
  # Create factors
  df <- df %>%
    dplyr::mutate(
      .question = make_factor(.data[[var]], var, vallab),
      .grp1     = make_factor(.data[[grp1]], grp1, vallab),
      .grp2     = if (nzchar(grp2))
        make_factor(.data[[grp2]], grp2, vallab)
      else factor(NA),
      .w        = if (nzchar(wgt) && wgt %in% names(data))
        suppressWarnings(as.numeric(.data[[wgt]]))
      else 1, 
      .filter_var     = if (nzchar(filter_var))
        make_factor(.data[[filter_var]], filter_var, vallab)
    ) %>%
    dplyr::mutate(.w = ifelse(is.na(.w) | .w < 0, 0, .w)) %>%
    dplyr::mutate(
      .group = if (nzchar(grp2)) {
        interaction(.grp1, .grp2, sep = " × ", drop = TRUE, lex.order = TRUE)
      } else {
        .grp1
      }
    )
  
  # Drop empty groups if requested
  if (isTRUE(drop_empty)) {
    df <- df %>% 
      dplyr::group_by(.group) %>% 
      dplyr::filter(any(!is.na(.question))) %>% 
      dplyr::ungroup()
  }
  
  # Get question text
  q_text <- varlab %>% 
    dplyr::filter(variable == var) %>% 
    dplyr::pull(label)
  if (length(q_text) == 0 || is.na(q_text)) q_text <- var
  
  list(df = df, question_text = q_text)
}

#' Create survey design
#' @param df Data frame with id and .w columns
#' @return Survey design object
create_survey_design <- function(df) {
  need_cols <- c("id", ".w", ".question", ".group")
  validate(need(
    all(need_cols %in% names(df)),
    paste("Internal data preparation failed. Missing:",
          paste(setdiff(need_cols, names(df)), collapse = ", "))
  ))
  
  survey::svydesign(ids = ~id, weights = ~.w, data = df)
}

#' Filter survey design by excluding response levels
#' @param design Survey design object
#' @param exclude_levels Character vector of levels to exclude
#' @return Filtered survey design
filter_survey_design <- function(design, exclude_levels) {
  if (length(exclude_levels) == 0) {
    return(design)
  }
  
  # Subset to remove excluded levels from outcome or grouping variables
  design <- subset(
    design,
    !(
      (.question %in% exclude_levels) |
        (.grp1 %in% exclude_levels) |
        (!is.na(.grp2) & (.grp2 %in% exclude_levels))
    )
  )
  
  # Drop unused factor levels
  design$variables$.question <- droplevels(design$variables$.question)
  design$variables$.grp1     <- droplevels(design$variables$.grp1)
  if (!all(is.na(design$variables$.grp2))) {
    design$variables$.grp2 <- droplevels(design$variables$.grp2)
  }
  design$variables$.group <- droplevels(design$variables$.group)
  
  design
}

#' Calculate weighted proportions
#' @param design Survey design object
#' @return Data frame with proportions
calculate_proportions <- function(design) {
  tab <- tryCatch(
    survey::svytable(~ .group + .question, design = design),
    error = function(e) {
      showNotification(paste("svytable error:", e$message), type = "error")
      NULL
    }
  )
  
  validate(need(!is.null(tab) && length(tab) > 0, 
                "No data to summarise for this selection."))
  
  ptab <- prop.table(tab, margin = 1)
  
  as.data.frame(ptab) %>%
    tibble::as_tibble() %>%
    dplyr::rename(group = `.group`, response = `.question`, prop = Freq) %>%
    dplyr::mutate(pct = 100 * prop) %>%
    dplyr::arrange(group, response)
}

#' Calculate unweighted Ns by group
#' @param design Survey design object
#' @return Data frame with counts
calculate_unweighted_ns <- function(design) {
  df <- design$variables
  df %>%
    dplyr::filter(!is.na(.question)) %>%
    dplyr::count(.group, name = "N_unweighted") %>%
    dplyr::arrange(.group)
}