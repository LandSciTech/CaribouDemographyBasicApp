
test_that("Can make figures", {
  inst_dir <- system.file(package = "CaribouDemographyBasicApp")
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = file.path(inst_dir, "extdata/translations"))
  i18n$set_translation_language("fr") # default translation to display

  set.seed(1234)
  surv_data <- rbind(bboudata::bbousurv_a, bboudata::bbousurv_b) %>%
    dplyr::filter(Year %in% c(2012:2016)) %>%
    dplyr::slice_sample(n = 30)
  recruit_data <- rbind(bboudata::bbourecruit_a, bboudata::bbourecruit_b) %>%
    dplyr::filter(Year %in% c(2012:2016)) %>%
    dplyr::slice_sample(n = 30)
  N0 <- 500
  pop_file_in <- caribouMetrics::bbouMakeSummaryTable(
    surv_data,
    recruit_data,
    N0 = N0,
    shiny_progress = FALSE, i18n = i18n, return_mcmc = TRUE,
    # hoping to make it faster...
    niters = 10)
  beepr::beep(3)
  pop_fits <- pop_file_in
  pop_file_in <- pop_file_in$parTab

  expect_s3_class(pop_file_in, "data.frame")

  tmp_fig_dir <- tempdir()
  unlink(file.path(tmp_fig_dir, "figures"), recursive = TRUE)
  dir.create(file.path(tmp_fig_dir, "figures"))
  bbouMakeFigures(pop_fits$surv_fit, pop_fits$recruit_fit,
                  fig_dir = file.path(tmp_fig_dir, "figures"),
                  i18n = i18n,
                  show_interpolated = FALSE)

  figs <- list.files(file.path(tmp_fig_dir, "figures"), pattern = "png", full.names = TRUE)

  if(interactive()){
    # open the figures, check for appropriate translation
    purrr::walk(figs, shell.exec)
  }


  purrr::walk(figs, \(x) expect_true(file.exists(x)))
})
