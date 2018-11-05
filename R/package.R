#' @keywords internal
"_PACKAGE"

#' Turn into a chemtools tbl df.
#' 
#' This is just so the units get displayed nicely in the header during printing to console.
#' 
#' @export
cht_df <- function(x) {
  x <- tbl_df(x)
  class(x) <- c("cht_tbl", class(x))
  return(x)
}

#' @export
format.cht_tbl <- function(x, ...) {
  is_q <- map_lgl(x, methods::is, "Quantity")
  ftbl <- x %>% dplyr::mutate_if(is_q, as.numeric) %>% tibble:::format.tbl(...)
  ftbl <- c(ftbl[1:2], assemble_tbl_units(ftbl[3], cht_get_units(x)), ftbl[-c(1:3)])
  return(ftbl)
}

assemble_tbl_units <- function(dt_line, units) {
  space <- stringr::str_match_all(dt_line, "(\\s+)([^ ]+)")[[1]] %>% { .[,2] } %>% nchar()
  data_type <- stringr::str_match_all(dt_line, "(\\s+)([^ ]+)")[[1]] %>% { .[,3] }
  stopifnot(length(space) == length(units))
  new_data_type <- map2_chr(
      data_type, units, 
      ~if (!is.na(.y)) { stringr::str_replace(.x, "<(dbl|int)>", .y) } else { .x }
    )
  new_space <- ifelse(!is.na(units), space + 5 - nchar(units, allowNA = TRUE), space) %>% 
      # Note: what to do if there is not enough space for the units?? right now just shrink space to 0
      # this will only happen if there are Quantity column names that are super short and units that are long (e.g. MW)
      { ifelse(. > 0, ., 0) }
  new_line <- map2_chr(new_space, new_data_type, ~paste0(paste(rep(" ", .x), collapse = ""), .y))
  return(paste(new_line, collapse = ""))
}

# allow quantity replication
#' @export
rep.Quantity <- function(x, ...) {
  x@.Data <- rep(x@.Data, ...)
  return(x)
}

#' @export
`[.Quantity` <- function(x, ...) {
  x@.Data <- `[`(x@.Data, ...)
  return(x)
}

#' @export
`[[.Quantity` <- function(x, ...) {
  x@.Data <- `[[`(x@.Data, ...)
  return(x)
}