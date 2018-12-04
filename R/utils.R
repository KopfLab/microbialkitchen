# helper functions that informs about operation error
operation_error <- function(operation, e1, e2) {
  stop(sprintf("%s is not implemented for these quantities (trying to use '%s' and '%s'). ", operation, class(e1)[1], class(e2)[1]), call. = FALSE)
}

class_check <- function(operation, e1, e2) {
  if (class(e1) != class(e2)) operation_error(operation, e1, e2)
}

require_quantity <- function(x_quo, check) {
  check_quo <- rlang::enquo(check)
  q_name <- sub("is_", "", rlang::quo_text(check_quo), fixed = TRUE)
  if (rlang::quo_is_missing(x_quo)) {
    stop(sprintf("parameter for %s is missing", q_name), call. = FALSE)
  }
  if (!check(rlang::eval_tidy(x_quo))) {
   stop(sprintf("'%s' is not a %s quantity", rlang::quo_text(x_quo), q_name), call. = FALSE)
  }
}