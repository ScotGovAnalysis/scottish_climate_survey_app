# functions/statistics.R
# Functions for statistical tests and comparisons

#' Prepare data for statistical analysis
#' @param data Raw data
#' @param vallab Value labels
#' @param var_stats Question variable
#' @param grp1_stats Grouping variable
#' @param wgt_stats Weight variable
#' @param var_map Variable grouping mappings
#' @param grp_map Group grouping mappings
#' @param exclude_levels Levels to exclude
#' @param drop_empty Drop empty groups flag
#' @return Prepared data frame
prepare_stats_data <- function(data, vallab, var_stats, grp1_stats, wgt_stats,
                               var_map, grp_map, exclude_levels, drop_empty) {
  # Build labelled factors
  df <- data %>%
    dplyr::mutate(
      .question = make_factor(.data[[var_stats]], var_stats, vallab),
      .group    = make_factor(.data[[grp1_stats]], grp1_stats, vallab)
    )
  
  # Collapse according to mappings
  df <- df %>%
    dplyr::mutate(
      .question = collapse_with_drop(.question, var_map),
      .group    = collapse_with_drop(.group, grp_map)
    ) %>%
    dplyr::filter(.question != "_DROP_", .group != "_DROP_")
  
  # Apply exclusions
  if (length(exclude_levels) > 0) {
    df <- df %>%
      dplyr::filter(
        !(.question %in% exclude_levels),
        !(.group %in% exclude_levels)
      )
  }
  
  # Drop empty groups
  if (isTRUE(drop_empty)) {
    df <- df %>% 
      dplyr::group_by(.group) %>% 
      dplyr::filter(any(!is.na(.question))) %>% 
      dplyr::ungroup()
  }
  
  # Add weights
  weight_col <- NULL
  if (!is.null(wgt_stats) && nzchar(wgt_stats) && wgt_stats %in% names(data)) {
    weight_col <- wgt_stats
  } else if ("@weight0" %in% names(data)) {
    weight_col <- "@weight0"
  }
  
  df <- df %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      .w = if (!is.null(weight_col)) suppressWarnings(as.numeric(.data[[weight_col]])) else 1
    ) %>%
    dplyr::mutate(.w = dplyr::if_else(is.na(.w) | .w < 0, 0, .w))
  
  # Validate and drop levels
  validate(need(sum(df$.w) > 0 && nrow(df) > 0, "No data remaining after filters/grouping."))
  
  df$.question <- droplevels(df$.question)
  df$.group    <- droplevels(df$.group)
  
  df
}

#' Run chi-squared test
#' @param design Survey design object
#' @return List with test results and proportions
run_chisq_test <- function(design) {
  # Weighted table and proportions
  wtab <- survey::svytable(~ .question + .group, design)
  validate(need(length(wtab) > 0, "No cells in contingency table."))
  ptab <- prop.table(wtab, margin = 2)
  
  # Convert to wide format
  props_df <- as.data.frame(ptab) %>%
    tibble::as_tibble() %>%
    dplyr::rename(response = `.question`, group = `.group`, prop = Freq)
  
  wide_props <- props_df %>%
    dplyr::select(response, group, prop) %>%
    tidyr::pivot_wider(names_from = group, values_from = prop, values_fill = 0) %>%
    dplyr::arrange(response)
  
  # Chi-square test
  chisq_res <- survey::svychisq(~ .question + .group, design = design, statistic = "F")
  p_val <- tryCatch(chisq_res$p.value, error = function(e) NA_real_)
  
  list(
    p_value = p_val,
    significant = !is.na(p_val) && p_val < 0.05,
    props_wide = wide_props
  )
}

#' Format chi-squared test result
#' @param chisq_result Result from run_chisq_test
#' @return Formatted text string
format_chisq_result <- function(chisq_result) {
  p_val <- chisq_result$p_value
  sig_text <- if (chisq_result$significant) {
    "Significant difference at alpha = 0.05"
  } else {
    "No significant difference at alpha = 0.05"
  }
  
  paste0(
    "Overall Chi-Square test (Rao–Scott)\n",
    "p-value: ", if (!is.na(p_val)) signif(p_val, 3) else "NA", "\n",
    sig_text
  )
}

#' Render chi-squared proportion table
#' @param wide_props Wide format proportions
#' @return DT datatable object
render_chisq_table <- function(wide_props) {
  pct_cols <- setdiff(names(wide_props), "response")
  DT::datatable(wide_props, options = list(scrollX = TRUE, dom = "tip"), rownames = FALSE) %>%
    DT::formatPercentage(columns = pct_cols, digits = 1)
}

#' Run pairwise comparisons
#' @param design Survey design object
#' @param alpha Significance level
#' @return Data frame with pairwise test results
run_pairwise_tests <- function(design, alpha = 0.05) {
  grp_lvls <- levels(droplevels(design$variables$.group))
  q_lvls   <- levels(droplevels(design$variables$.question))
  
  pairs <- utils::combn(grp_lvls, 2, simplify = FALSE)
  res_list <- list()
  
  for (pair in pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    
    sub_dsgn <- subset(design, .group %in% c(g1, g2))
    
    for (y in q_lvls) {
      sub_dsgn$variables$outcome_ind <- as.numeric(sub_dsgn$variables$.question == y)
      
      by_formula <- as.formula("~.group")
      by_fit <- survey::svyby(~outcome_ind, by_formula, survey::svymean, design = sub_dsgn)
      
      if (!all(c(g1, g2) %in% by_fit$.group)) next
      
      co <- coef(by_fit)[c(g1, g2)]
      V  <- vcov(by_fit)[c(g1, g2), c(g1, g2)]
      
      dvec <- c(1, -1)
      diff_est <- sum(dvec * co)
      diff_se  <- sqrt(drop(dvec %*% V %*% dvec))
      z <- diff_est / diff_se
      p <- 2 * (1 - pnorm(abs(z)))
      
      res_list[[length(res_list) + 1]] <- data.frame(
        Comparison = paste(g1, "vs", g2),
        Outcome    = y,
        Diff       = diff_est,
        SE         = diff_se,
        Raw_p      = p
      )
    }
  }
  
  res_df <- dplyr::bind_rows(res_list)
  res_df %>%
    dplyr::mutate(
      Bonferroni_p = p.adjust(Raw_p, method = "bonferroni"),
      Significant_05 = ifelse(Bonferroni_p < alpha, "Yes", "No"),
      Diff_pct = round(100 * Diff, 1),
      SE_pct   = round(100 * SE, 1)
    )
}

#' Render pairwise comparison table
#' @param pairwise_result Result from run_pairwise_tests
#' @return DT datatable object
render_pairwise_table <- function(pairwise_result) {
  DT::datatable(
    pairwise_result %>%
      dplyr::select(Comparison, Outcome, Diff_pct, SE_pct, Raw_p, Bonferroni_p, Significant_05),
    options = list(
      scrollX = TRUE,
      dom = "tip",
      paging = FALSE,
      pageLength = nrow(pairwise_result)
    ),
    rownames = FALSE
  ) %>%
    DT::formatRound(columns = c("Raw_p", "Bonferroni_p"), digits = 4)
}