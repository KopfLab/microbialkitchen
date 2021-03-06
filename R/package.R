#' @keywords internal
"_PACKAGE"

#' @importFrom rlang !! .data
#' @importFrom methods is new setOldClass
#' @importFrom vctrs vec_ptype2 vec_ptype2.double vec_ptype2.character vec_cast vec_cast.double vec_cast.character vec_arith vec_arith.numeric
NULL

#' @importFrom tibble tibble
#' @export
tibble::tibble

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# quiets concerns of R CMD check about . in pipelineds
# and .data in tidyverse functions
utils::globalVariables(".")