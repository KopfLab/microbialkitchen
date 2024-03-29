% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/units.R
\name{quantity_units}
\alias{quantity_units}
\alias{get_qty_units}
\alias{get_qty_units_with_label}
\title{Work with quantity units.}
\usage{
get_qty_units(q)

get_qty_units_with_label(q, label = names(q))
}
\arguments{
\item{q}{quantities}

\item{label}{text label to use with the units - single value or vector of the same length as \code{q}. By default uses the names of \code{q}, which only works if \code{q} is a list or data frame.}
}
\description{
Work with quantity units.
}
\section{Functions}{
\itemize{
\item \code{get_qty_units()}: get units from a quantity, list of quantities or data frame (returns NA for objects/columns that are not quantities)

\item \code{get_qty_units_with_label()}: get units from a quantity, list of quantities or data frame, with a custom label in the format \code{label [units]}. Objects/columns that are not quantities simply return the label with out the [units] part.

}}
\examples{

# quantity units examples
qty(5000, "g") \%>\% get_qty_units()
x <- list(a = qty(5000, "g"), b = 42, c = qty(100, "mbar"))
x \%>\% get_qty_units()
# labels with units
get_qty_units_with_label(qty(0.1, "mM"), "concentration")

# make labels with units for data frame columns
x <- data.frame(a = qty(1, "mg"), b = 2, c = qty(100, "mbar"))
get_qty_units_with_label(x)
get_qty_units_with_label(x, "same label")
}
\seealso{
Other quantity functions: 
\code{\link{check_quantities}},
\code{\link{make_qty_units_explicit}()},
\code{\link{metric_scaling}},
\code{\link{quantities}},
\code{\link{quantity_data}}
}
\concept{quantity functions}
