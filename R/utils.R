# Null default
# Analog of || from ruby
#
# @name nulldefault-infix
# @author Hadley Wickham
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


# "Invert" a list
# Keys become values, values become keys
#
# @param list to invert
# @author Hadley Wickham
invert <- function(L) {
  t1 <- unlist(L)
  names(t1) <- rep(names(L), lapply(L, length))
  tapply(names(t1), t1, c)
}

# Inside
# Return logical vector indicating if x is inside the interval
#
# @author Hadley Wickham
"%inside%" <- function(x, interval) {
  x >= interval[1] & x <= interval[2]
}
