library(shinytest2)
library(CaribouDemographyBasicApp)

# Note these tests use the installed version so must build and install before testing changes

# this is away of creating the code for a test interactively:
# shinytest2::record_test(run_caribou_demog_app())


test_that("Update data works properly", {
  # skip("Skipping takes too long")
  skip_on_ci()
  skip_on_covr()
  skip_if_not_installed("googlesheets4")

  shiny_app <- run_caribou_demog_app()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE),
                       load_timeout = 60*1000, timeout = 20*1000)
  app$set_window_size(width = 1619, height = 1065, wait = FALSE)
  app$click("update_data")
  Sys.sleep(2)
  app$set_inputs(survey_url = "https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing", wait_ = FALSE)
  Sys.sleep(2)
  app$click("update_data_submit", wait_ = FALSE)
  app$wait_for_idle(timeout = 8*60*1000)

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

# Check app works and take snapshots to use in vignettes
vig_pics <- here::here("vignettes/snapshots")

# delete because can't overwrite
unlink(vig_pics, recursive = TRUE)
dir.create(vig_pics)

inst_dir <- system.file(package = "CaribouDemographyBasicApp")
i18n <- shiny.i18n::Translator$new(translation_csvs_path = file.path(inst_dir, "extdata/translations"))
langs <- i18n$get_languages()
to_test <- data.frame(lang = langs,
                      altname = c("Increase recruitment", "More calves",
                                  "Augmentation le recrutement", "Plus de faons"))

# do for all languages
purrr::pmap(to_test, \(lang, altname){
  test_that("App loads properly", {
    skip_on_ci()
    skip_on_covr()

    shiny_app <- run_caribou_demog_app()
    app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE),
                         seed = 1234,
                         load_timeout = 60*1000, timeout = 20*1000)

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
      pop_name = "A",
      cur_pop = character(0),
      addl_params_cur = character(0),
      ivType = "logistic",
      S_bar = 83.3,
      R_bar = 24,
      S_iv_mean = 0.217863520723237,
      R_iv_mean = 0.0974887866343633,
      S_iv_shape = 11.6,
      R_iv_shape = 25.9,
      P_0_cur = 1,
      P_K_cur = 0.6,
      s_cur = 0.5,
      N0 = 175,
      S_sd = 0.238,
      R_sd = 0.081,
      a_cur = 1,
      b_cur = 4,
      K_cur = 10000,
      r_max_cur = 1.3,
      wait_ = FALSE
    )

    app$set_inputs(body = "input_data_tab", wait_ = FALSE)

    # doing this twice to try and force different values of rates to make vignette text work
    # app$set_inputs(cur_pop = "Estimated demographic parameters")
    app$set_inputs(
      S_bar = 83.3,
      R_bar = 24,
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

    alt_scns <- tidyr::tribble(
      ~altname,          ~R_bar, ~S_bar,
      "85% Fem 24% Calv", 24,   85,
      "85% Fem 35% Calv", 35,   85,
      "90% Fem 24% Calv", 24,   90,
      "90% Fem 35% Calv", 35,   90,
    )
    alt_scns$alt_n <- 1:4

    if(stringr::str_detect(lang, "fr")){
      alt_scns$altname <-  stringr::str_replace(alt_scns$altname, "Calv", "Faon")
    }

    purrr::pwalk(alt_scns,\(altname, R_bar, S_bar, alt_n){
      app$click("add_alt")
      app$wait_for_idle()
      # app$set_inputs(alt_box_1 = "alt_box_p_1")

      input_lst <- list(
        alt_S_bar_1 = S_bar,
        alt_R_bar_1 = R_bar,
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

      names(input_lst) <- stringr::str_replace(names(input_lst), "_1", paste0("_", alt_n))

      do.call(app$set_inputs, input_lst)
    })

    app$click("run_model")
    app$wait_for_idle()
    Sys.sleep(5)

    app$get_screenshot(file.path(vig_pics, paste0("alt_results_", lang, ".png")))

    pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
      stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
      stringr::str_replace_all("\n;", "\n") %>%
      {read.table(text = ., sep = ";", header = TRUE)}

    # expect_equal(pop_tbl[2,1], alt_scns$altname[1])

    pop_nm <- app$get_text("#pop_name")
    expect_equal(pop_nm, "\nA")

    app$stop()

  })
})
