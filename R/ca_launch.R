#' Launch a caladaptr demo app
#'
#' Launch a caladaptr demo Shiny app
#'
#' @param app App name
#' @param display.mode How to display the Shiny app
#'
#' @details
#'
#' @import caladaptr
#' @import conflicted
#' @import dplyr
#' @import DT
#' @import ggplot2
#' @import htmltools
#' @import leaflet
#' @import lubridate
#' @import rmarkdown
#' @import scales
#' @import shiny
#' @import shinybusy
#' @import shinyhelper
#' @import tibble
#' @import tidyr
#' @import units

#' @export

ca_launch <- function(app = c("timeseries", "chill", "chill2")[1], 
                      display.mode = c("normal", "showcase")[1]) {

  if (length(app) != 1) stop("app should be lenth 1", call. = FALSE)
  
  appDir <- system.file(paste0("shiny/", app), package = "caladaptr.apps")
  
  if (appDir == "") {
    stop(paste0("Could not find the `", app, "` app. Try re-installing `caladaptr.apps`."), call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = display.mode)

}

