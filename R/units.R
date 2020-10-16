#' Work with quantity units.
#'
#' @name quantity_units
#' @family quantity functions
#' @param q quantities
NULL 


#' Make units explicit
#'
#' This function is intended for data frames / tibbles only and makes the specified columns explicit with the target units in the column name.
#'
#' @param df the data frame in which to make the units explicit
#' @param ... named arguments for the columns that should be made explicit and their units, e.g. \code{salt_weight = "mg", salt_concentration = "M"}.
#' @param prefix the prefix for the units
#' @param suffix the suffix for the units
#' @family quantity functions
#' @examples
#' # data frame with quantities
#' df <- tibble(x = qty(1:5, "mg"), y = qty(1:5, "L"))
#' df
#'
#' # show with explicit units
#' make_units_explicit(df, x = "mg")
#' make_units_explicit(df, x = "g", y = "mL")
#' @export
make_units_explicit <- function(df, ..., prefix = " [", suffix = "]") {
  
  # columns
  cols <- list(...)
  col_names <- names(cols)
  
  # safety checks
  if(!is.data.frame(df)) stop("can only make units explicit in data frames", call. = FALSE)
  if (length(cols) == 0) return(df)
  if (length(dups <- which(duplicated(col_names))) > 0) {
    stop("can only specify one unit per column, found multiple for: ", 
         paste(names(cols[dups]), collapse = ", "), call. = FALSE)
  }
  if (length(missing <- setdiff(col_names, names(df))) > 0) {
    stop("columns do not exist in this data frame: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  
  # make units explicit
  cols_idx <- match(col_names, names(df))
  new_cols <- sprintf("%s%s%s%s", col_names, prefix, get_qty_units(df[col_names]), suffix)
  df[cols_idx] <- purrr::map2(df[cols_idx], cols, get_qty_value)
  names(df)[cols_idx] <- new_cols
  return(df)
}


#' @describeIn quantity_units get units from a quantity, list of quantities or data frame (returns NA for objects/columns that are not quantities)
#' @examples
#' 
#' # quantity units examples
#' qty(5000, "g") %>% get_qty_units()
#' x <- list(a = qty(5000, "g"), b = 42, c = qty(100, "mbar"))
#' x %>% get_qty_units()
#' @export
get_qty_units <- function(q) {
  if (is_qty(q))
    return(attr(q, "unit"))
  else if (is.list(q))
    return(purrr::map_chr(q, ~if(is_qty(.x)) { attr(.x, "unit") } else { NA_character_ }))
  else
    return(NA_character_)
}

#' @describeIn quantity_units get units from a quantity, list of quantities or data frame, with a custom label in the format \code{label [units]}. Objects/columns that are not quantities simply return the label with out the [units] part.
#' @param label text label to use with the units - single value or vector of the same length as \code{q}. By default uses the names of \code{q}, which only works if \code{q} is a list or data frame.
#' @examples
#' # labels with units
#' get_qty_units_with_label(qty(0.1, "mM"), "concentration")
#'
#' # make labels with units for data frame columns
#' x <- data.frame(a = qty(1, "mg"), b = 2, c = qty(100, "mbar"))
#' get_qty_units_with_label(x)
#' get_qty_units_with_label(x, "same label")
#' @export
get_qty_units_with_label <- function(q, label = names(q)) {
  units <- get_qty_units(q)
  if (length(label) == 1 || length(label) == length(units)) {
    
  } else {
    sprintf("incompatible number of labels (%d) provided for units (%d)", length(label), length(units)) %>%
      stop(call. = FALSE)
  }
  return(ifelse(is.na(units), label, sprintf("%s [%s]", label, units)))
}
