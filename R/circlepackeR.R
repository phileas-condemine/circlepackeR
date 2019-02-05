#' htmlwidget for d3.js zoomable circle packing visualizations
#'
#' \href{http://bl.ocks.org/mbostock/7607535}{Circle packing visualizations} provide
#' an interactive way of exploring hierarchical data that contains elements of different
#' sizes or magnitudes
#'
#' @param data data in the form of a hierarchical list or a nested d3 JSON hierarchy
#' @param size string representing the name of the size variable.  \code{"size"} is
#'          the default.
#' @param color_min string representing the minimum value of the color range for the
#'          circles. The string can be either a hexadecimal, RGB, or HSL color.
#'          \code{"hsl(152,80\%,80\%)"} is the default.
#' @param color_max string representing the maximum value of the color range for the
#'          circles. The string can be either a hexadecimal, RGB, or HSL color.
#'          \code{"hsl(228,30\%,40\%)"} is the default.
#'
#' @example ./inst/examples/example.R
#'
#' @import htmlwidgets
#'
#' @export
circlepackeR <- function(data, size = "size", color_min = "hsl(152,80%,80%)",
                         color_max = "hsl(228,30%,40%)", width = NULL, height = NULL) {

  # accept JSON
  if (inherits(data, c("character", "connection", "json"))) {
    data = jsonlite::toJSON(
      jsonlite::fromJSON(data),
      auto_unbox = TRUE,
      dataframe = "rows"
    )

  } else if (inherits(data, "list")) {  # accept hierarchical list
    data = jsonlite::toJSON(data, auto_unbox = TRUE)

  } else if (inherits(data, "Node")) { # accept data.tree
    if (!requireNamespace("data.tree")) stop("please install data.tree.", call. = FALSE)
    data = as.list(data, mode = "explicit", unname = TRUE)
    data = jsonlite::toJSON(data, auto_unbox = TRUE)

  } else {
    stop("Please provide a json object or list", call. = FALSE)
  }

  # create a list that contains the data
  x = list(
    data = data,
    options = list(
      size = size,
      color_min = color_min,
      color_max = color_max
    )
  )

  # create widget
  libs=c('stdlib.js'
  ,'src/html.js'
  ,'src/index.js'
  ,'src/library.js'
  ,'src/md.js'
  ,'src/mutable.js'
  ,'src/now.js'
  ,'src/promises/delay.js'
  ,'src/promises/index.js'
  ,'src/promises/tick.js'
  ,'src/promises/when.js'
  ,'src/require.js'
  ,'src/resolve.js'
  ,'src/svg.js'
  ,'src/template.js'
  ,'src/tex.js'
  ,'src/that.js'
  ,'src/width.js'
  ,'src/dom/canvas.js'
  ,'src/dom/context2d.js'
  ,'src/dom/download.js'
  ,'src/dom/element.js'
  ,'src/dom/index.js'
  ,'src/dom/input.js'
  ,'src/dom/range.js'
  ,'src/dom/select.js'
  ,'src/dom/svg.js'
  ,'src/dom/text.js'
  ,'src/dom/uid.js'
  ,'src/files/buffer.js'
  ,'src/files/index.js'
  ,'src/files/text.js'
  ,'src/files/url.js'
  ,'src/generators/disposable.js'
  ,'src/generators/filter.js'
  ,'src/generators/index.js'
  ,'src/generators/input.js'
  ,'src/generators/map.js'
  ,'src/generators/observe.js'
  ,'src/generators/queue.js'
  ,'src/generators/range.js'
  ,'src/generators/valueAt.js'
  ,'src/generators/worker.js'
  ,'src/constant.js')

  dep=htmltools::htmlDependency("observablehq_stdlib","2.0.8","htmlwidgets/lib/observablehq/stdlib/",
                 script= libs,package = "circlepackeR")

  htmlwidgets::createWidget(
    name = 'circlepackeR',
    x,
    width = width,
    height = height,
    package = 'circlepackeR',
    dependencies=dep
  )
}

#' Widget output function for use in Shiny
#'
#' @export
circlepackeROutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'circlepackeR', width, height, package = 'circlepackeR')
}

#' Widget render function for use in Shiny
#'
#' @export
renderCirclepackeR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, circlepackeROutput, env, quoted = TRUE)
}
