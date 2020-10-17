#' ggplot scales for quantities
#'
#' These are the scales for microbial kitchen quantities.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_x_continuous
#' @param unit which unit to use for the axis. If provided, the values will be converted to this unit before plotting. Otherwise the current unit of the variable on the axis will be used.
#' @param each whether to label each axis break (\code{each = TRUE}) or the axis title as a whole (default).
#'
#' @name scale_qty
NULL

#' @importFrom ggplot2 scale_type
#' @export
ggplot2::scale_type

#' @rdname scale_qty
#' @format NULL
#' @usage NULL
#' @export
scale_type.mk_quantity <- function(x) c('qty', 'continuous')

# TODO: add examples (see operations vignette)
#' @importFrom scales censor
#' @importFrom ggplot2 waiver
#' @rdname scale_qty
#' @export
scale_x_qty <- function(
  name = waiver(), breaks = waiver(), unit = NULL, each = FALSE,
  minor_breaks = waiver(), n.breaks = NULL,
  labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
  na.value = NA_real_, trans = 'identity', guide = waiver(),
  position = 'bottom', sec.axis = waiver()) {
  
  # units
  if (!is.null(unit) && !is.character(unit)) {
    if (is_qty(unit)) unit <- get_qty_units(unit)
    else stop("unit must be either NULL, a character or a quantity", call. = FALSE)
  }

  # generate scale
  sc <- ggplot2::continuous_scale(
    c('x', 'xmin', 'xmax', 'xend', 'xintercept', 'xmin_final', 'xmax_final',
      'xlower', 'xmiddle', 'xupper'),
    "position_c", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPositionQty
  )
  sc$unit <- unit
  sc$each <- each
  
  # secondary axis
  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_qty
#' @export
scale_y_qty <- function(
  name = waiver(), breaks = waiver(), unit = NULL, each = FALSE,
  minor_breaks = waiver(), n.breaks = NULL,
  labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
  na.value = NA_real_, trans = 'identity', guide = waiver(),
  position = 'left', sec.axis = waiver()) {
  
  # units
  if (!is.null(unit) && !is.character(unit)) {
    if (is_qty(unit)) unit <- get_qty_units(unit)
    else stop("unit must be either NULL, a character or a quantity", call. = FALSE)
  }
  
  # generate scale
  sc <- ggplot2::continuous_scale(
    c('y', 'ymin', 'ymax', 'yend', 'yintercept', 'ymin_final', 'ymax_final',
      'lower', 'middle', 'upper'),
    "position_c", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousPositionQty
  )
  sc$unit <- unit
  sc$each <- each
  
  # secondary axis
  set_sec_axis(sec.axis, sc)
}

# currently not exported by ggplot2 so reproduced
set_sec_axis <- function(sec.axis, scale) {
  if (!inherits(sec.axis, 'waiver')) {
    if (inherits(sec.axis, 'formula')) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!inherits(sec.axis, 'AxisSecondary')) stop("Secondary axes must be specified using 'sec_axis()'", call. = FALSE)
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}

#' @rdname scale_qty
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousPositionQty <- ggplot2::ggproto(
  'ScaleContinuousPositionQty', ggplot2::ScaleContinuousPosition,
  unit = NULL, each = FALSE,
  
  transform = function(self, x) {
    if (is_qty(x)) {
      self$data_unit <- get_qty_units(x)
      self$scale_unit <- self$data_unit
      if (!is.null(self$unit)) self$scale_unit <- self$unit
      x <- get_qty_value(x, self$scale_unit)
    }
    ggproto_parent(ScaleContinuousPosition, self)$transform(x)
  },
  get_labels = function(self, breaks = self$get_breaks()) {
    if (self$each && !is.null(self$data_unit) && inherits(self$labels, 'waiver')) {
      self$labels <- function(breaks) {
        if (!is.null(self$unit)) {
          qty(breaks, self$data_unit) %>% get_qty_text(unit = self$scale_unit)
        } else {
          qty(breaks, self$data_unit) %>% get_qty_text_each()
        }
      }
    }
    ggproto_parent(ScaleContinuousPosition, self)$get_labels(breaks)
  },
  make_title = function(self, title) {
    if (!self$each && !is.null(self$scale_unit)) {
      return(sprintf("%s [%s]", title, self$scale_unit))
    }
    return(title)
  }
)

