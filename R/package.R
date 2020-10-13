#' @keywords internal
"_PACKAGE"

#' @importFrom rlang !! .data
#' @importFrom methods is new
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom forcats as_factor
#' @export
forcats::as_factor

# quiets concerns of R CMD check about . in pipelineds
# and .data in tidyverse functions
utils::globalVariables(".")