# helper functions that informs about operation error
operation_error <- function(operation, e1, e2) {
  stop(sprintf("%s is not implemented for these quantities (trying to use '%s' and '%s'). ", operation, class(e1)[1], class(e2)[1]), call. = FALSE)
}

class_check <- function(operation, e1, e2) {
  if (class(e1) != class(e2)) operation_error(operation, e1, e2)
}

require_Class <- function(class, x) {
  #if ()
}