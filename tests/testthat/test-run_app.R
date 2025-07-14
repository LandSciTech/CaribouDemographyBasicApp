library(shinytest2)
library(CaribouDemographyBasicApp)

# shinytest2::record_test(run_caribou_demog_app())

test_that("App loads properly", {
  skip_on_ci()
  skip_on_covr()

  shiny_app <- run_caribou_demog_app()
  app <- AppDriver$new(shiny_app, variant = platform_variant(r_version = FALSE), seed = 1234)

  app$set_window_size(width = 1700, height = 1400)
  Sys.sleep(2)
  app$wait_for_idle()
  app$expect_screenshot(name = "welcome")

  app$set_inputs(body = "input_data_tab")
  app$wait_for_idle()
  app$expect_screenshot(name = "input_data")

  app$set_window_size(width = 1619, height = 1065)
  app$set_inputs(
    pop_name = "A",
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

  n <- ""
  # n <- 1
  # n <- n+1
  app$set_window_size(width = 1000, height = 1000)
  app$set_inputs(dimension = c(10000,1000), allow_no_input_binding_ = TRUE)
  app$click("run_model")
  app$wait_for_idle()
  app$expect_screenshot(name = paste0("pop_plot", n), selector = "#pop_plot")


  app$set_window_size(width = 1619, height = 1565)
  app$expect_screenshot(name = paste0("pop_table", n), selector = "#pop_table_card")
  app$expect_screenshot(name = paste0("r_m_plot", n), selector = "#r_m_plot")

  app$expect_screenshot(name = paste0("full_results", n))
  # shell.exec(paste0("C:/Users/ENDICO~1/AppData/Local/Temp/RtmpIREbKJ/st2-685c62b657b6/full_results", n, ".png"))


  pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
    stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
    stringr::str_replace_all("\n;", "\n") %>%
    {read.table(text = ., sep = ";", header = TRUE)}

  expect_equal(pop_tbl$Scenario[1], "Current")


  app$set_window_size(width = 1619, height = 1565)
  app$click("add_alt")
  app$wait_for_idle()
  # app$set_inputs(alt_box_1 = "alt_box_p_1")
  app$set_inputs(
    alt_S_bar_1 = 88,
    alt_R_bar_1 = 29,
    alt_name_1 = "Increase recruitment",
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

  app$expect_screenshot(name = paste0("alt_results", n))

  pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
    stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
    stringr::str_replace_all("\n;", "\n") %>%
    {read.table(text = ., sep = ";", header = TRUE)}

  expect_equal(pop_tbl$Scenario[2], "Increase recruitment")
  app$stop()

})

test_that("Update data works properly", {
  skip("Skipping takes too long")
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
  pop_tbl <- app$get_text("#pop_table") %>% stringr::str_replace_all("    ", ";") %>%
    stringr::str_replace_all("   ", ";") %>% stringr::str_replace_all("  ", ";") %>%
    stringr::str_replace_all("\n;", "\n") %>%
    {readr::read_delim(., delim = ";", skip_empty_rows = TRUE)}

  pop_nm <- app$get_text("#pop_name")

  expect_equal(pop_nm, "\nA")
  app$stop()
})
