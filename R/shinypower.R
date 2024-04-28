#' shinypower
#'
#' An interactive app for the power of a distribution
#'
#' @return Plot of the Power vs the sample size n
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinypower()}
shinypower <- function() {
  runApp(system.file("shinypower",
                     package = "MATH4753ROSAlab12"),
         launch.browser = TRUE)
}
