rd <- getwd() # Root repo directory

# Run app
browser_path <- file.path(rd, "chrome", "chrome.exe")
if (file.exists(browser_path)) {

  # Open in embedded browser (must use a specific port)
  shiny::runApp(
    appDir = file.path(wd),
    launch.browser = function(shinyurl) {
      system(paste0("\"", browser_path, "\" --app=", shinyurl, " -incognito"), wait = F)
    }
  )

} else {
  message("running caribou demography app")

  # Open in default web browser
  library(CaribouDemographyBasicApp)
  run_caribou_demog_app(lang = "fr", private = TRUE, allow_update_data = FALSE)

}


