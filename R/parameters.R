
# List of valid parameters
.parameters <- list(
  temperature = "Temperature",
  solution.volume = "Volume",
  headspace.volume = "Volume",
  headspace.pressure = "Pressure"
)

#' Reports whether x is a parameters aquasys object
#' @export
is.parameter <- function(x) inherits(x, "parameters")


#' Set system parameters
#'
#' Use this function to modify system parameters
#'
#' To see a graphical representation of the inheritance tree, see the
#' last example below.
#' @param ... a list of parameter name and value pairings that modify
#' an existing aquatic system
#' @export
parameter <- function(...) {
  params <- list(...)
  
  # Check that all parameters have the correct class
  mapply(validate_parameter, params, names(params))
  
  structure(list(parameters = params), class = c("parameters", "aquasys"))
}

# Add parameters from an aquasys to another.
add_parameter <- function (p1, p2) {
  if (!is.aquasys(p1) || !is.aquasys(p2))
    stop ("This function can only work with aquasys objects") # should not happen
  
  if (is.null(p1$parameters))
    p1$parameters <- list()
  
  if (!is.null(p2$parameters))
    p1$parameters <- modifyList(p1$parameters, p2$parameters)
  
  return(p1)
}

# Checks the parameter (value p, name pname)
# against the .paramters list of allowed
# system paramters and throws an error if
# there are any problems.
validate_parameter <- function(p, pname) {
  
  pclass <- .parameters[[pname]]
  
  if (is.null(pclass)) {
    stop('"', pname, '" is not a valid parameter.')
  }
  
  if (!inherits(p, pclass) ) {
    stop('"', pname, '" must be a ', pclass, '.')
  }
  invisible()
}

