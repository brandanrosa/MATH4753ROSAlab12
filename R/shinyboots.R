#' shinyboots
#'
#' An interactive app with a slider for the alpha value
#'
#' @return Histograms of the log(lambda) and Ybar distributions with vertical line indicating the cut-off value for acceptance/rejection regions
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinyboots()}
shinyboots <- function() {
  runApp(system.file("shinyboots",
                     package = "MATH4753ROSAlab12"),
         launch.browser = TRUE)
}
