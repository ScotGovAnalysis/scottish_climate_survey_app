# functions/grouped_analysis.R
# Functions for grouped (multi-select) variable analysis

#' Find unique '@' prefixed question roots
#' @param data Data frame
#' @param varlab Variable labels
#' @return Data frame with root and choice_lab columns
find_grouped_prefixes <- function(data, varlab) {
  # Find variables starting with '@' and containing underscore
  at_vars <- names(data)[stringr::str_detect(names(data), "^@.+_")]
  
  if (length(at_vars) == 0) {
    return(tibble::tibble(root = character(0), choice_lab = character(0)))
  }
  
  # Extract roots (part before underscore)
  roots <- unique(sub("^(.*?)(_.*)$", "\\1", at_vars))
  
  # Get labels for each root
  root_df <- lapply(roots, function(r) {
    # Try exact match in Variable Labels
    lab <- varlab %>% 
      dplyr::filter(variable == r) %>% 
      dplyr::pull(label)
    
    if (length(lab) == 0 || is.na(lab[1]) || !nzchar(lab[1])) {
      # Try any child label
      lab <- varlab %>%
        dplyr::filter(stringr::str_detect(variable, paste0("^", stringr::fixed(r), "_"))) %>%
        dplyr::pull(label) %>% 
        unique()
    }
    
    if (length(lab) == 0 || is.na(lab[1]) || !nzchar(lab[1])) lab <- r
    
    tibble::tibble(root = r, choice_lab = lab[1])
  }) %>% 
    dplyr::bind_rows()
  
  root_df %>% dplyr::arrange(choice_lab)
}

#' Get child variables for a grouped root
#' @param root Root variable name (e.g., '@qa3')
#' @param data Data frame
#' @return Character vector of child variable names
grouped_children <- function(root, data) {
  vars <- names(data)[stringr::str_detect(names(data), paste0("^", stringr::fixed(root), "_"))]
  
  # Sort by numeric suffix if present
  ord <- suppressWarnings(as.integer(sub(".*_(\\d+)$", "\\1", vars)))
  vars[order(ord, vars)]
}

#' Get labels for child variables (value==1)
#' @param children Child variable names
#' @param vallab Value labels
#' @return Named character vector of labels
child_one_labels <- function(children, vallab) {
  sapply(children, function(v) {
    lab <- vallab %>% 
      dplyr::filter(variable == v, value == "1") %>% 
      dplyr::pull(label)
    
    if (length(lab) == 0 || is.na(lab[1]) || !nzchar(lab[1])) v else lab[1]
  }, USE.NAMES = TRUE)
}

#' Compute grouped analysis with significance tests
#' @param data Raw data
#' @param varlab Variable labels
#' @param vallab Value labels
#' @param var_grouped_root Root variable
#' @param grp1_grouped Grouping variable
#' @param wgt_grouped Weight variable
#' @param exclude_levels_grouped Levels to exclude
#' @param drop_empty_groups_grouped Drop empty groups flag
#' @return List with wide_df and other results
compute_grouped_analysis <- function(data, varlab, vallab, var_grouped_root,
                                     grp1_grouped, wgt_grouped,
                                     exclude_levels_grouped, drop_empty_groups_grouped) {
  # Get children
  children <- grouped_children(var_grouped_root, data)
  
  validate(need(
    length(children) > 0,
    paste("No child variables found for", var_grouped_root)
  ))
  
  # Build grouping factor
  df <- data %>%
    dplyr::mutate(.group = make_factor(.data[[grp1_grouped]], grp1_grouped, vallab))
  
  # Create indicator columns (before any filtering)
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(children),
        ~ ifelse(is.na(.), NA_real_, as.numeric(as.character(.) == "1"))
      )
    )
  
  # Add weights BEFORE creating survey design
  weight_col <- NULL
  if (!is.null(wgt_grouped) && nzchar(wgt_grouped) && wgt_grouped %in% names(data)) {
    weight_col <- wgt_grouped
  } else if ("@weight0" %in% names(data)) {
    weight_col <- "@weight0"
  }
  
  df <- df %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      .w = if (!is.null(weight_col)) suppressWarnings(as.numeric(.data[[weight_col]])) else 1
    ) %>%
    dplyr::mutate(.w = dplyr::if_else(is.na(.w) | .w < 0, 0, .w))
  
  # Create survey design FIRST
  dsg <- survey::svydesign(ids = ~id, weights = ~.w, data = df)
  
  # NOW apply exclusions to the survey design
  if (length(exclude_levels_grouped) > 0) {
    dsg <- subset(dsg, !(.group %in% exclude_levels_grouped))
  }
  
  # Drop empty groups if requested (subset the design)
  if (isTRUE(drop_empty_groups_grouped)) {
    # Create a variable indicating if row has any non-missing child responses
    dsg$variables$any_non_missing <- rowSums(
      !is.na(dsg$variables[, children, drop = FALSE]), 
      na.rm = TRUE
    )
    # Keep only groups that have at least one row with non-missing responses
    grp_counts <- dsg$variables %>%
      dplyr::group_by(.group) %>%
      dplyr::summarise(has_data = any(any_non_missing > 0), .groups = "drop") %>%
      dplyr::filter(has_data)
    dsg <- subset(dsg, .group %in% grp_counts$.group)
    # Clean up temporary variable
    dsg$variables$any_non_missing <- NULL
  }
  
  # Drop unused factor levels
  dsg$variables$.group <- droplevels(dsg$variables$.group)
  
  # Validate
  validate(need(
    nrow(dsg$variables) > 0 && sum(dsg$variables$.w) > 0,
    "No data remaining after filters/grouping."
  ))
  
  # Formula for svyby
  fml <- stats::as.formula(
    paste("~", paste(sprintf("`%s`", children), collapse = " + "))
  )
  
  by_grp <- survey::svyby(fml, ~ .group, survey::svymean, design = dsg, na.rm = TRUE)
  
  # Clean names
  names(by_grp) <- gsub("`", "", names(by_grp))
  
  present_means <- intersect(children, names(by_grp))
  
  validate(need(
    length(present_means) > 0,
    "svyby returned no mean columns (all missing?). Try different root or filters."
  ))
  
  # Run significance tests on the survey design
  df_for_test <- dsg$variables
  make_yesno <- function(x) {
    factor(ifelse(x == 1, "Yes", ifelse(is.na(x), NA, "No")), levels = c("No", "Yes"))
  }
  
  for (v in children) {
    df_for_test[[paste0(v, "_yesno")]] <- make_yesno(df_for_test[[v]])
  }
  
  dsg_sig <- survey::svydesign(ids = ~id, weights = ~.w, data = df_for_test)
  
  pvals <- sapply(children, function(v) {
    fml <- as.formula(paste0("~", paste0("`", v, "_yesno`"), " + .group"))
    out <- survey::svychisq(fml, design = dsg_sig, na.rm = TRUE)
    pv <- tryCatch(out$p.value, error = function(e) NA_real_)
    pv
  })
  
  pvals_adj <- p.adjust(pvals, method = "holm")
  names(pvals_adj) <- sub("\\.X-squared$", "", names(pvals_adj))
  
  # Get labels and add significance markers
  opt_labels <- child_one_labels(present_means, vallab)
  sig_mark <- ifelse(is.na(pvals_adj[present_means]), "",
                     ifelse(pvals_adj[present_means] < 0.05, "★ ", ""))
  opt_labels_marked <- paste0(unname(opt_labels), sig_mark)
  
  # Build wide table
  wide_df <- by_grp %>%
    tibble::as_tibble() %>%
    dplyr::select(.group, dplyr::any_of(present_means)) %>%
    { colnames(.) <- c(".group", opt_labels_marked); . } %>%
    dplyr::mutate(dplyr::across(-.group, ~ round(100 * ., 1))) %>%
    dplyr::arrange(.group)
  
  list(wide_df = wide_df, pvals = pvals_adj)
}

#' Render grouped table
#' @param wide_df Wide format data frame
#' @return DT datatable object
render_grouped_table <- function(wide_df) {
  pct_cols <- setdiff(names(wide_df), ".group")
  DT::datatable(
    wide_df,
    options = list(scrollX = TRUE, dom = "t", paging = FALSE),
    rownames = FALSE
  ) %>%
    DT::formatStyle(".group", fontWeight = "bold")
}

#' Create grouped heatmap plot
#' @param wide_df Wide format data frame
#' @return ggplot object
create_grouped_plot <- function(wide_df) {
  validate(need(nrow(wide_df) > 0, "No data to plot."))
  
  long_df <- wide_df %>%
    tidyr::pivot_longer(cols = -.group, names_to = "option", values_to = "pct")
  
  ggplot2::ggplot(long_df, ggplot2::aes(x = option, y = .group, fill = pct)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "% selected") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

#' Debug grouped children (print diagnostics)
#' @param var_grouped_root Root variable
#' @param data Data frame
debug_grouped_children <- function(var_grouped_root, data) {
  message("\n[DEBUG] var_grouped_root changed to: ", var_grouped_root)
  
  ch <- grouped_children(var_grouped_root, data)
  
  message("[DEBUG] grouped_children returned ", length(ch), " names:")
  print(ch)
  
  message("[DEBUG] Names(data):")
  print(names(data))
  
  missing <- setdiff(ch, names(data))
  if (length(missing) > 0) {
    message("[DEBUG] Missing (not in data): ", paste(missing, collapse = ", "))
  } else {
    message("[DEBUG] All children exist in data.")
  }
}