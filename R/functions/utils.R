# functions/utils.R
# General utility functions

#' Notification helper
#' @param txt Message text
#' @param type Type of notification ("message", "warning", "error")
#' @param duration Duration in seconds
notify <- function(txt, type = "message", duration = 5) {
  showNotification(txt, type = switch(type,
                                      "error" = "error",
                                      "warning" = "warning",
                                      "message"
  ), duration = duration)
}

#' Infix operator for NULL/empty string handling
#' @param x Value to check
#' @param y Default value if x is NULL or empty
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.character(x) && !nzchar(x)) return(y)
  x
}

#' Create safe filename from text
#' @param text Original text
#' @param prefix Filename prefix
#' @param extension File extension
#' @return Safe filename string
create_safe_filename <- function(text, prefix, extension) {
  safe <- gsub("[^A-Za-z0-9_\\-]+", "_", text)
  paste0(prefix, "_", safe, ".", extension)
}

#' Collect group mappings from input
#' @param input Shiny input object
#' @param name_prefix Prefix for name inputs
#' @param levels_prefix Prefix for levels inputs
#' @param n_groups Number of groups to check
#' @return Named list of group mappings
collect_group_mappings <- function(input, name_prefix, levels_prefix, n_groups) {
  mapping_list <- list()
  for (i in 1:n_groups) {
    nm <- input[[paste0(name_prefix, i)]]
    lv <- input[[paste0(levels_prefix, i)]]
    if (is.character(nm) && nzchar(nm) && length(lv) > 0) {
      mapping_list[[nm]] <- lv
    }
  }
  mapping_list
}