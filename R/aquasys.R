# make sure classes are loaded before everything else
#' @include classes.R
NULL

#' Reports whether x is any aquasys object (system, parameter or component)
#' @export
is.aquasys <- function(x) inherits(x, "aquasys")

#' Reports whether x is a system object
#' @export
is.system <- function(x) inherits(x, "system")

# FIXME better readme

#' Modify an aquatic system by adding/replacing parameters and components
#' 
#' @param e1 An object of class \code{aquasys} 
#' @param e2 A parameter or component to add to \code{e1}
#' 
#' @export
#' @method + aquasys
#' @rdname aquasys-add
"+.aquasys" <- function(e1, e2) {
  # pass along name of e2 for error messages
  e2name <- deparse(substitute(e2))
  
  # for now, don't allow this, not sure if it makes sense to allow...
  if (is.system(e2))
    stop("Can't add a system to another system, only parameters and components")
  

  if (is.component(e2)) {
    # could be a single component
    
  } else if (is.parameter(e2)) {
    
    # list of parameters
    if (!is.null(e2$parameters))
      e1 <- add_parameter(e1, e2)
    
    # list of components
    if (!is.null(e2$components)) {
      e1 <- add_component(e1, e2)
    }
  
  } else {
    stop("Don't know how to add ", e2name, " to an aquasys.")
  }
  
  return(e1)
}



#' Create a new aquatic system
#' 
#' @export
aquasys <- function(...) UseMethod("aquasys")

#' @export
aquasys.default <- function(...) {
  # defaults
  sys <- parameter(
      temperature = cht_qty(25, "C"),
      solution.volume = cht_qty(1, "L"),
      headspace.volume = cht_qty(Inf, "L", scale = F),
      headspace.pressure = cht_qty(1, "atm")
    ) + parameter(...)

  sys$components <- list(
    solids = list(),
    gases = list()
  )
  
  attr(sys, "class") <- c("system", "aquasys")
  return(sys)
}

#' @method print system
#' @export
print.system <- function(x, ...) {
  message("trying to compute and print this aquatic system!")
  invisible(NULL)
}

