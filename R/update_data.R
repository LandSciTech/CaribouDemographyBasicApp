#' Get updated data from google spreadsheet
#'
#' @param survey_url sheet url
#' @param save_dir dir where results are saved
#' @param shiny_progress Is this inside a shiny app and called with `shiny::withProgress`
#' @param i18n shiny.i18n translator
#' @param lang language to select in translator
#'
#' @returns A data frame with the population estimates.
#' @export
#'
#' @examples
#' googlesheets4::gs4_deauth()
#' update_data("https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing",
#'             save_dir = tempdir())
update_data <- function(survey_url, save_dir = tools::R_user_dir("CaribouDemographyBasicApp", "data"),
                        i18n = NULL, lang = "en",
                        shiny_progress = FALSE){

  if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  if(!dir.exists(file.path(save_dir, "www"))) dir.create(file.path(save_dir, "www"))
  if(!dir.exists(file.path(save_dir, "extdata"))) dir.create(file.path(save_dir, "extdata"))

  # set plot theme
  theme_set(
    theme_classic()+
      theme(text = element_text(size = 20),
            plot.caption = element_text(hjust = 0))
  )

  if(is.null(i18n)){
    i18n <- list(t = function(x, session = NULL)paste0(x),
                 get_languages = function(x)"en",
                 get_translation_language = function(x)"en",
                 set_translation_language = function(x)"en")
  }

  start <- Sys.time()

  data_in <- dataFromSheets(survey_url = survey_url, shiny_progress = shiny_progress,
                            i18n = i18n)

  # assigns each element of the list to the current env
  list2env(data_in, envir = environment())

  # Make sure there is disturbance for each year
  all_years <- union(survey_surv$Year, survey_recruit$Year)
  dist <- fillDisturbance(dist,
                          expand.grid(Year = all_years,
                                      PopulationName = unique(dist$PopulationName)))

  survey_surv <- survey_surv %>% filter(PopulationName %in% pops_run)
  survey_recruit <- survey_recruit %>% filter(PopulationName %in% pops_run)

  if(max(dist$Year) <= max(survey_surv$Year)){
    # set future disturbance to most recent and project to a year with no observations
    # add a missing year of data so we have a projection in a future year
    year_add <- max(survey_surv$Year) + 1
    survey_surv <- addMissingYears(survey_surv, year_add)
    survey_recruit <- addMissingYears(survey_recruit, year_add)
    dist <- dist %>% ungroup() %>%
      bind_rows(
        dist %>% ungroup() %>%
          summarise(.by = PopulationName,
                    Year = year_add,
                    Anthro = max(Anthro),
                    Fire_excl_anthro = mean(Fire_excl_anthro))
      )
  }

  pop_file_in <- estimateBayesianRates(
    survey_surv, survey_recruit,
    disturbance = dist,
    N0 = N0 %>% filter(PopulationName %in% pops_run) %>%
      rename(N.lower = FemalePopulationLower, N.upper = FemalePopulationUpper),
    shiny_progress = shiny_progress, i18n = i18n, return_mcmc = TRUE)

  pop_fits <- pop_file_in
  res_list <- pop_file_in$parList

  #filter out populations with only one year of data at this stage
  rmPops <- unique(c(
    (survey_surv %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName,
    (survey_recruit %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName))

  pops_run <- setdiff(pops_run,rmPops)

  inlang <- i18n$get_translation_language()

  sess <- shiny::getDefaultReactiveDomain()
  sess$userData$shiny.i18n$lang <- NULL

  purrr::walk(i18n$get_languages(), \(x){
    i18n$set_translation_language(x)
    bbouMakeFigures(bayesianResults = pop_fits,
                    fig_dir = file.path(save_dir, "www"),
                    i18n = i18n, sess = sess,
                    show_interpolated = FALSE)
  })

  i18n$set_translation_language(inlang)

  # Get data into long format for saving

  # Get the estimate in the future year that was added to be used as expected
  # future rates for all years in the app
  res_list$Rbar <- filter(res_list$Rbar, Annual == year_add)
  res_list$Sbar <- filter(res_list$Sbar, Annual == year_add)

  res_tbl <- res_list %>% purrr::keep_at(c("Recruitment", "Survival", "Sbar", "Rbar")) %>%
    bind_rows() %>%
    # Remove monthly survival if all the same
    distinct(mean, sd, lower, upper, PopulationName, Annual, MetricTypeID,
             .keep_all = TRUE)

  # add rows for interannual var. They are different for bbou vs beta models
  if(pop_fits$parList$type == "beta"){
    res_tbl <- res_tbl %>%
      add_row(lower = pop_fits$parList$Siv$S_cv_min,
              upper = pop_fits$parList$Siv$S_cv_max,
              MetricTypeID = "S_cv") %>%
      add_row(lower = pop_fits$parList$Riv$R_cv_min,
              upper = pop_fits$parList$Riv$R_cv_max,
              MetricTypeID = "R_cv")
  } else if (pop_fits$parList$type == "bbou"){
    res_tbl <- res_tbl %>%
      add_row(mean = pop_fits$parList$Siv$S_iv_mean,
              shape = pop_fits$parList$Siv$S_iv_shape,
              MetricTypeID = "S_iv") %>%
      add_row(mean = pop_fits$parList$Riv$R_iv_mean,
              shape = pop_fits$parList$Riv$R_iv_shape,
              MetricTypeID = "R_iv")
  } else {
    stop("model type not supported")
  }

  surv_data_amt <- pop_fits$surv_fit$data %>% filter(!is.na(MortalitiesCertain)) %>%
    group_by(PopulationName, Year) %>%
    summarise(nCollars = max(StartTotal)) %>%
    summarise(nCollarYears = sum(nCollars),
              nSurvYears = n_distinct(Year))

  recruit_data_amt <- pop_fits$recruit_fit$data %>% filter(!is.na(Cows)) %>%
    group_by(PopulationName, Year) %>%
    summarise(nCows = sum(Cows)) %>%
    summarise(nCowsAllYears = sum(nCows),
              nRecruitYears = n_distinct(Year))

  data_amt <- merge(surv_data_amt, recruit_data_amt) %>%
    pivot_longer(-PopulationName, names_to = "MetricTypeID", values_to = "mean")

  N0 <- N0 %>% rename(lower = FemalePopulationLower, upper = FemalePopulationUpper) %>%
    mutate(MetricTypeID = "FemalePopulation")

  dat_desc <- dat_desc %>% pivot_longer(everything(), names_to = "MetricTypeID",
                            values_to = "PopulationName")

  pop_file_in <- bind_rows(res_tbl, dat_desc, data_amt, N0)

  end <- Sys.time()
  print(end - start)

  # save the file locally so only re-run when asked
  write.csv(pop_file_in, file.path(save_dir, "extdata/temp_pop_file_local.csv"), row.names = FALSE)

  pop_file_in

}
