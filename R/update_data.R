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

  pop_file_in <- estimateBayesianRates(
    survey_surv %>% filter(PopulationName %in% pops_run),
    survey_recruit %>% filter(PopulationName %in% pops_run),
    N0 = N0 %>% filter(PopulationName %in% pops_run),
    shiny_progress = shiny_progress, i18n = i18n, return_mcmc = TRUE)

  pop_fits <- pop_file_in
  pop_file_in <- pop_file_in$parTab

  #filter out populations with only one year of data at this stage
  rmPops <- unique(c(
    (survey_surv %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName,
    (survey_recruit %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName))

  pops_run <- setdiff(pops_run,rmPops)

  pop_file_in <- subset(pop_file_in, is.element(pop_name,pops_run))

  inlang <- i18n$get_translation_language()

  sess <- shiny::getDefaultReactiveDomain()
  sess$userData$shiny.i18n$lang <- NULL

  purrr::walk(i18n$get_languages(), \(x){
    i18n$set_translation_language(x)
    bbouMakeFigures(pop_fits$surv_fit, pop_fits$recruit_fit,
                    fig_dir = file.path(save_dir, "www"),
                    i18n = i18n, sess = sess,
                    show_interpolated = FALSE)
  })

  i18n$set_translation_language(inlang)

  pop_file_in <- bind_cols(pop_file_in, dat_desc)

  end <- Sys.time()
  print(end - start)

  # save the file locally so only re-run when asked
  write.csv(pop_file_in, file.path(save_dir, "extdata/temp_pop_file_local.csv"), row.names = FALSE)

  pop_file_in

}
