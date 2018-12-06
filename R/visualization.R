#' Quantity labeller function
#' 
#' This is a useful labeller function for ggplots to generate quantity text labels on discrete scales and facets in the used created by \code{\link{get_qty_text}}. Supports both best metric for each label (simply don't provide the \code{unit} parameter) or a specify unit (specify the \code{unit} parameter). 
#' 
#' @param q the quantity for which to generate a text labeller
#' @param unit the unit (or units) in which to represent the quantity on the plot. By default uses the best metric prefix for each value of the quantity, which is good for generating orders of magnitude scales e.g. from mg to g to kg. If provided, uses the specified unit or units (need to be compatible with the quantity) for all values.
#' @inheritParams get_qty_text
#' @examples 
#' 
#' 
#' # dual unit labels
#' q <- qty(seq(0, 1000, by = 100), "K")
#' l <- make_qty_text_labeller(q, unit = c("K", "C"))
#' l(q) # test how the labels would look
#' @export
make_qty_text_labeller <- function(q, unit = NULL, signif = 3) {
  if (missing(q) || !is_qty(q)) stop("requires a quantity to generate a labeller for", call. = FALSE)
  
  # make sure unit is valid
  if (!is.null(unit)) {
    purrr::map(unit, ~get_qty_value(q, .x))
  }
  
  # keep track of base unit
  base_unit <- get_qty_units(q)
  
  labeller <- function(labels, ...) {
    # make compatible with both wrap and scale labeling
    panel_labels <- FALSE
    if (is(labels, "data.frame")) {
      panel_labels <- TRUE
      labels <- labels[[1]]
    }
    
    # generate labels
    qs <- purrr::map(labels, qty, base_unit)
    if (is.null(unit)) 
      labels <- purrr::map_chr(qs, get_qty_text) 
    else 
      labels <- qs %>% purrr::map_chr(
        ~paste(purrr::map2_chr(.x, unit, ~get_qty_text(.x, .y, signif = signif)), collapse = "\n"))
    
    if (panel_labels) labels <- list(labels)
    return(labels)
  }
  
  return(structure(labeller, class = "labeller"))
}