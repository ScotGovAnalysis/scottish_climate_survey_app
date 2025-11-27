# functions/plotting.R
# Functions for creating plots and visualizations

#' Likert palette helper
#' @param n Number of colors needed
#' @param scheme Color scheme name
#' @param reverse Whether to reverse the palette
#' @return Character vector of colors
likert_palette <- function(n, scheme = c("RdYlGn", "PuOr", "RdBu", "BrBG"), reverse = FALSE) {
  scheme <- match.arg(scheme)
  
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    pal  <- RColorBrewer::brewer.pal(max(3, min(11, max(3, n))), scheme)
    cols <- grDevices::colorRampPalette(pal)(max(1, n))
  } else {
    # Fallback to base R diverging palettes
    cols <- grDevices::hcl.colors(max(1, n), palette = scheme, rev = FALSE)
  }
  
  if (isTRUE(reverse)) rev(cols) else cols
}

#' Create proportion plot
#' @param prop_df Proportions data frame
#' @param palette Palette name
#' @param palette_reverse Whether to reverse palette
#' @return ggplot object
create_proportion_plot <- function(prop_df, palette, palette_reverse) {
  validate(need(nrow(prop_df) > 0, 
                "No non-missing data to plot. Try changing the question/exclusions."))
  
  # Enforce factor ordering
  dfp <- prop_df %>%
    dplyr::mutate(
      group    = forcats::fct_inorder(as.factor(group)),
      response = forcats::fct_inorder(as.factor(response))
    )
  
  # Get palette
  n_levels <- nlevels(dfp$response)
  validate(need(n_levels > 0, "No response levels remain after exclusions; adjust filters."))
  
  raw_scheme   <- palette %||% "RdYlGn"
  scheme       <- if (identical(raw_scheme, "BuRd")) "RdBu" else raw_scheme
  reverse_flag <- xor(isTRUE(palette_reverse), identical(raw_scheme, "BuRd"))
  
  likert_cols <- likert_palette(n = n_levels, scheme = scheme, reverse = reverse_flag)
  validate(need(
    length(likert_cols) == n_levels,
    sprintf("Palette length mismatch (got %d colours for %d levels)",
            length(likert_cols), n_levels)
  ))
  
  # Build plot
  ggplot2::ggplot(dfp, ggplot2::aes(x = prop, y = group, fill = response)) +
    ggplot2::geom_col(width = 0.7, colour = "white", size = 0.2) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_fill_manual(values = likert_cols, name = NULL,
                               guide = ggplot2::guide_legend(reverse = FALSE)) +
    ggplot2::labs(x = "Proportion of group", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.box         = "vertical"
    )
}

#' Create wide table from proportions
#' @param prop_df Proportions data frame
#' @param ns Unweighted Ns data frame
#' @return Wide format data frame
create_wide_table <- function(prop_df, ns) {
  wide_pct <- prop_df %>%
    dplyr::select(group, response, prop) %>%
    dplyr::arrange(group, response) %>%
    tidyr::pivot_wider(names_from = response, values_from = prop, values_fill = 0) %>%
    dplyr::arrange(group)
  
  n_group <- ns %>% dplyr::rename(group = .group)
  
  wide_pct %>%
    dplyr::left_join(n_group, by = "group") %>%
    dplyr::relocate(N_unweighted, .after = group)
}

#' Render proportion table for DT
#' @param wide_table Wide format table
#' @return DT datatable object
render_proportion_table <- function(wide_table) {
  pct_cols <- names(wide_table)[!(names(wide_table) %in% c("group", "N_unweighted"))]
  
  DT::datatable(
    wide_table, rownames = FALSE,
    options = list(
      paging = FALSE,
      dom = "t",
      scrollX = TRUE
    )
  ) %>%
    DT::formatPercentage(columns = pct_cols, digits = 1) %>%
    DT::formatStyle(1, fontWeight = "bold")
}