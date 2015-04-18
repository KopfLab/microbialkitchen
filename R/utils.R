#' Null default
#' Analog of || from ruby
#'
#' @name nulldefault-infix
#' @author Hadley Wickham
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


