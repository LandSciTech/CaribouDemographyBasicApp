rd <- file.path(getwd(), "app_files") # Root repo directory
message(rd)

# Rtools is needed for nimble to work
Sys.setenv("RTOOLS42_HOME" = file.path(rd, "/rtools/rtools42"))

r = getOption("repos")
r["CRAN"] = "https://cran.rstudio.com/"
options(repos = r)

# install.packages("pkgbuild", type = "binary")
# pkgbuild::check_build_tools(debug = TRUE)



# remove installed packages to get a clean slate
lib_use <- file.path(rd, "R/library")
unlink(file.path(lib_use, "00LOCK"), recursive = TRUE)
# pkgs_rm <- installed.packages(lib.loc = lib_use, priority = NA_character_)[,1]
# message(pkgs_rm)
# remove.packages(pkgs_rm, lib_use)

if(!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes", type = "binary", )
}
# installs the app and all it's dependencies
remotes::install_github("LandSciTech/CaribouDemographyBasicApp@dev",
                        type = "binary", upgrade = "always")

# Update the data
library(CaribouDemographyBasicApp)
googlesheets4::gs4_auth(email = TRUE)
# Quebec survey url
# File with translations
inst_dir <- system.file(package = "CaribouDemographyBasicApp")
i18n <- shiny.i18n::Translator$new(translation_csvs_path = file.path(inst_dir, "extdata/translations"))
i18n$set_translation_language("fr") # default translation to display


def_survey_url <- "https://docs.google.com/spreadsheets/d/1FJZ06dc1-oKUEsNrrjSfqlEn3MGsXODQhJ3gmbqmp-s/edit?usp=sharing"
message("updating caribou demography app data")
update_data(def_survey_url, i18n = i18n, save_dir = inst_dir)


