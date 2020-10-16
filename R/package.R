#' @keywords internal
"_PACKAGE"

#' @importFrom rlang !! .data
#' @importFrom tibble tibble
#' @importFrom methods is new setOldClass
#' @importFrom vctrs vec_ptype2 vec_ptype2.double vec_ptype2.character vec_cast vec_cast.double vec_cast.character vec_arith vec_arith.numeric
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