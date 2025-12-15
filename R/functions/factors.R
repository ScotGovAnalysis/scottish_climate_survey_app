# functions/factors.R
# Functions for creating and manipulating factors with value labels

#' Create factor with value labels
#' @param x Vector to convert
#' @param varname Variable name for looking up labels
#' @param vallab Value labels data frame
#' @return Factor with appropriate labels
make_factor <- function(x, varname, vallab) {
  # Treat negative codes as missing
  if (is.numeric(x)) {
    x <- ifelse(x < 0, NA, x)
  }
  
  # Look up value labels
  vl <- vallab %>% dplyr::filter(variable == varname)
  
  if (nrow(vl) == 0) {
    return(forcats::fct_inorder(as.factor(x)))
  } else {
    lvl_codes <- as.character(vl$value)
    lvl_labs  <- as.character(vl$label)
    return(factor(as.character(x), levels = lvl_codes, labels = lvl_labs))
  }
}

#' Collapse factor levels with dropping of unassigned levels
#' @param fct Factor to collapse
#' @param mapping_list Named list of new level names to old level vectors
#' @return Factor with collapsed levels
collapse_with_drop <- function(fct, mapping_list) {
  # If no mapping provided, just drop unused levels
  if (length(mapping_list) == 0) {
    return(forcats::fct_drop(fct))
  }
  
  # Get all levels user chose to keep
  keep_levels <- unique(unlist(mapping_list, use.names = FALSE))
  
  # Mark unassigned levels as "_DROP_"
  f_other <- forcats::fct_other(fct, keep = keep_levels, other_level = "_DROP_")
  
  # Collapse kept levels into user-specified names
  f_collapsed <- suppressWarnings(forcats::fct_collapse(f_other, !!!mapping_list))
  
  forcats::fct_drop(f_collapsed)
}

#' Get levels for a variable
#' @param varname Variable name
#' @param vallab Value labels data frame
#' @param data Data frame
#' @return Character vector of levels
get_variable_levels <- function(varname, vallab, data) {
  lvls <- vallab %>% 
    dplyr::filter(variable == varname) %>% 
    dplyr::pull(label)
  
  if (length(lvls) == 0) {
    lvls <- unique(as.character(data[[varname]]))
  }
  
  lvls
}