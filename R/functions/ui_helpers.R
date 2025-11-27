# functions/ui_helpers.R
# Functions for creating UI elements

#' Create display labels for variables
#' @param vars Variable names
#' @param varlab Variable labels data frame
#' @param vallab Value labels data frame
#' @return Character vector of display labels
make_display_labels <- function(vars, varlab, vallab) {
  sapply(vars, function(v) {
    # Base label from Variable Labels
    base_label <- varlab$label[varlab$variable == v]
    if (length(base_label) == 0 || is.na(base_label)) base_label <- v
    
    # If child variable (starts with @ and has underscore), append Value=1 label
    if (grepl("^@", v) && grepl("_", v)) {
      val1_label <- vallab %>%
        dplyr::filter(variable == v, value == "1") %>%
        dplyr::pull(label)
      if (length(val1_label) > 0 && nzchar(val1_label)) {
        return(paste0(v, ": ", base_label, " ", val1_label))
      }
    }
    
    # Default: just base label
    paste0(v, ": ", base_label)
  }, USE.NAMES = FALSE)
}

#' Create variable select UI
#' @param wb Workbook reactive value
#' @param var_choices Variable choices data frame
#' @param input_id Input ID for the select input
#' @param label Label for the select input
#' @return UI element
create_var_select_ui <- function(wb, var_choices, input_id, label) {
  req(wb)
  
  if (nrow(var_choices) == 0) {
    return(tagList(
      h5(label),
      div(class = "text-danger", "No variables found.")
    ))
  }
  
  display_labels <- make_display_labels(var_choices$variable, wb$varlab, wb$vallab)
  
  selectInput(
    input_id, label,
    choices = setNames(var_choices$variable, display_labels),
    selected = var_choices$variable[1],
    width = "100%"
  )
}

#' Create weight select UI
#' @param wb Workbook reactive value
#' @param weight_choices Weight choices data frame
#' @param input_id Input ID for the select input
#' @return UI element
create_weight_select_ui <- function(wb, weight_choices, input_id) {
  req(wb)
  
  if (nrow(weight_choices) == 0) {
    return(selectInput(
      input_id, "Weighting variable:",
      choices = c("Equal weights (1)" = ""),
      selected = "",
      width = "100%"
    ))
  }
  
  selectInput(
    input_id, "Weighting variable:",
    choices = c("Equal weights (1)" = "", setNames(weight_choices$variable, weight_choices$choice_lab)),
    selected = "",
    width = "100%"
  )
}

#' Create question header UI
#' @param question_text Question text
#' @param varlab Variable labels data frame
#' @param grp1 Primary grouping variable
#' @param grp2 Secondary grouping variable
#' @param wgt Weighting variable
#' @param show_counts Whether to show counts
#' @param ns Unweighted Ns data frame
#' @param exclude_levels Excluded levels
#' @return UI element
create_question_header <- function(question_text, varlab, grp1, grp2, wgt, 
                                   show_counts, ns, exclude_levels) {
  req(question_text)
  
  # Get labels
  grp1_lbl <- varlab$label[match(grp1, varlab$variable)]
  if (is.na(grp1_lbl)) grp1_lbl <- grp1
  
  grp2_lbl <- if (nzchar(grp2)) {
    lbl <- varlab$label[match(grp2, varlab$variable)]
    if (is.na(lbl)) grp2 else lbl
  } else NULL
  
  wgt_lbl <- if (nzchar(wgt)) {
    lbl <- varlab$label[match(wgt, varlab$variable)]
    if (is.na(lbl)) wgt else lbl
  } else "Equal weights (1)"
  
  # Counts text
  ns_text <- NULL
  if (isTRUE(show_counts) && nrow(ns) > 0) {
    ns_text <- paste0(
      " • Unweighted Ns: ",
      paste0(ns$.group, "=", ns$N_unweighted, collapse = "; ")
    )
  }
  
  # Exclusions text
  excl_text <- NULL
  if (length(exclude_levels) > 0) {
    excl_text <- paste0(" • Excluded responses: ", paste(exclude_levels, collapse = ", "))
  }
  
  tagList(
    h4(question_text),
    p(
      strong("Grouping: "),
      if (nzchar(grp2)) paste0(grp1_lbl, " × ", grp2_lbl) else grp1_lbl, br(),
      strong("Weight: "), wgt_lbl,
      if (!is.null(excl_text)) excl_text,
      if (!is.null(ns_text)) ns_text
    )
  )
}