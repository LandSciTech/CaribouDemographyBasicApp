library(shinytest2)
library(CaribouDemographyBasicApp)

# Note these tests use the installed version so must build and install before testing changes

# shinytest2::record_test(run_caribou_demog_app())

vig_pics <- here::here("vignettes/snapshots")

# delete because can't overwrite
unlink(vig_pics, recursive = TRUE)
dir.create(vig_pics)

inst_dir <- system.file(package = "CaribouDemographyBasicApp")
i18n <- shiny.i18n::Translator$new(translation_csvs_path = file.path(inst_dir, "extdata/translations"))

to_test <- data.frame(lang = i18n$get_languages(),
                      altname = c("Increase recruitment", "Augmentation le recrutement"))

# do for both languages
purrr::pmap(to_test, \(lang, altname){
  test_that("App loads properly", {
    skip_on_ci()
    skip_on_covr()

    shiny_app <- run_caribou_demog_app()
    app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE), seed = 1234)

    app$set_inputs(selected_language = lang, wait_ = FALSE)

    app$set_window_size(width = 1700, height = 1400, wait = FALSE)
    Sys.sleep(5)
    app$wait_for_idle()
    app$get_screenshot(file.path(vig_pics, paste0("welcome_", lang, ".png")))

    app$set_inputs(body = "input_data_tab")
    app$wait_for_idle()
    app$get_screenshot(file.path(vig_pics, paste0("input_data_", lang, ".png")))

    app$set_window_size(width = 1619, height = 1065)
    app$set_inputs(
      pop_name = "B",
      cur_pop = character(0),
      addl_params_cur = character(0),
      ivType = "logistic",
      S_bar = 84.0694082727134,
      R_bar = 26.7153340763145,
      S_iv_cv = 0.217863520723237,
      R_iv_cv = 0.0974887866343633,
      S_iv_sd = 1.24256062006226,
      R_iv_sd = 0.694032575040565,
      P_0_cur = 1,
      P_K_cur = 0.6,
      s_cur = 0.5,
      N0 = 175,
      S_sd = 0.513,
      R_sd = 0.141,
      a_cur = 1,
      b_cur = 4,
      K_cur = 10000,
      r_max_cur = 1.3,
      wait_ = FALSE
    )

    app$set_window_size(width = 1000, height = 1000)
    app$set_inputs(dimension = c(10000,1000), allow_no_input_binding_ = TRUE)
    app$click("run_model")
    app$wait_for_idle()
    app$get_screenshot(file.path(vig_pics, paste0("pop_plot_", lang, ".png")), selector = "#pop_plot")


    app$set_window_size(width = 1619, height = 1565)
    app$get_screenshot(file.path(vig_pics, paste0("pop_table_", lang, ".png")), selector = "#pop_table_card")
    app$get_screenshot(file.path(vig_pics, paste0("r_m_plot_", lang, ".png")), selector = "#r_m_plot")

    app$get_screenshot(file.path(vig_pics, paste0("full_results_", lang, ".png")))
    # shell.exec(paste0("C:/Users/ENDICO~1/AppData/Local/Temp/RtmpIREbKJ/st2-685c62b657b6/full_results", n, ".png"))


    pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
      stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
      stringr::str_replace_all("\n;", "\n") %>%
      {read.table(text = ., sep = ";", header = TRUE)}

    expect_true(pop_tbl[1,1] %in% c("Current", "Actuel"))


    app$set_window_size(width = 1619, height = 1565)
    app$click("add_alt")
    app$wait_for_idle()
    # app$set_inputs(alt_box_1 = "alt_box_p_1")
    app$set_inputs(
      alt_S_bar_1 = 88,
      alt_R_bar_1 = 29,
      alt_name_1 = altname,
      addl_params_1 = character(0),
      P_0_1 = 1,
      P_K_1 = 0.6,
      s_1 = 0.5,
      a_1 = 1,
      b_1 = 4,
      K_1 = 10000,
      r_max_1 = 1.3,
      wait_ = FALSE
    )
    app$click("run_model")
    app$wait_for_idle()
    Sys.sleep(5)

    app$get_screenshot(file.path(vig_pics, paste0("alt_results_", lang, ".png")))

    pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
      stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
      stringr::str_replace_all("\n;", "\n") %>%
      {read.table(text = ., sep = ";", header = TRUE)}

    expect_equal(pop_tbl[2,1], altname)

    pop_nm <- app$get_text("#pop_name")
    expect_equal(pop_nm, "\nB")

    app$stop()

  })
})

test_that("Update data works properly", {
  # skip("Skipping takes too long")
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("googlesheets4")

  shiny_app <- run_caribou_demog_app()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE))
  app$set_window_size(width = 1619, height = 1065)
  app$click("update_data")
  app$set_inputs(survey_url = "https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing", wait_ = FALSE)
  app$click("update_data_submit", wait_ = FALSE)
  app$wait_for_idle(timeout = 7*60*1000)

  app$click("run_model", wait_ = FALSE)
  Sys.sleep(5)
  pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
    stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
    stringr::str_replace_all("\n;", "\n") %>%
    {read.table(text = ., sep = ";", header = TRUE)}

  pop_nm <- app$get_text("#pop_name")

  expect_equal(pop_nm, "\nA")
  app$stop()
})
