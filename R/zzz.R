# tell shiny where to find images
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "www",
      package = "CaribouDemographyBasicApp"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
}
