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
#' update_data("https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing")
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
    i18n <- list(t = function(x)paste0(x),
                 get_languages = function(x)"en")
  }

  start <- Sys.time()

  sh_name <- googlesheets4::gs4_get(survey_url)$name
  if(shiny_progress && !rlang::is_installed("shiny")){
    warning("Package shiny is not installed. Setting shiny_progress to FALSE")
    shiny_progress <- FALSE
  }

  if(shiny_progress) shiny::setProgress(0.1, message = paste0(i18n$t("Downloading data from "), sh_name))
  survey_sh_names <- googlesheets4::sheet_names(survey_url)

  recruit_sh <- stringr::str_subset(survey_sh_names, "[R,r]ecruit")
  if(length(recruit_sh)<1){
    stop("The spreadsheet does not include a sheet named recruit")
  }
  nms <- c("PopulationName", "Year", "Month", "Day", "Cows",
           "Bulls", "UnknownAdults", "Yearlings", "Calves")
  survey_recruit <- googlesheets4::read_sheet(survey_url, recruit_sh,
                                              na = "NA") %>%
    select(any_of(nms)) %>%
    filter(if_all(everything(), \(x)!is.na(x))) %>%
    bboudata::bbd_chk_data_recruitment(multi_pops = TRUE)

  # Error in make bbouSummary table if only 1 year
  #survey_recruit <- survey_recruit %>% group_by(PopulationName) %>%
  #  filter(n_distinct(Year) > 1)

  surv_sh <- stringr::str_subset(survey_sh_names, "[S,s]urv")
  if(length(surv_sh)<1){
    stop("The spreadsheet does not include a sheet named 'surv'")
  }
  survey_surv <- googlesheets4::read_sheet(survey_url, surv_sh,
                                           na = "NA") %>%
    bboudata::bbd_chk_data_survival(multi_pops = TRUE, allow_missing = TRUE)

  #survey_surv <- survey_surv %>% group_by(PopulationName) %>%
  #  filter(n_distinct(Year) > 1)

  pop_sh <- stringr::str_subset(survey_sh_names, "[P,p]opulation")
  if(length(pop_sh)<1){
    stop("The spreadsheet does not include a sheet named 'population'")
  }

  nms <- c("PopulationName", "Year", "FemalePopulationLower", "FemalePopulationUpper")

  survey_pop <- googlesheets4::read_sheet(survey_url, pop_sh,
                                          na = "NA") %>%
    select(any_of(nms))

  pop_nms <- purrr::map_lgl(nms,
                            \(x)stringr::str_detect(colnames(survey_pop), x) %>% any())

  if(!all(pop_nms)){
    stop("The population estimates sheet is missing the expected column names:",
         paste0(colnames(survey_pop)[!pop_nms], collapse = ", "))
  }

  N0 <- survey_pop %>% group_by(PopulationName) %>% filter(Year == max(Year))

  pops_run <- intersect(survey_recruit$PopulationName,
                        survey_surv$PopulationName) %>%
    intersect(N0$PopulationName)

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

  # Add description
  desc_sh <- stringr::str_subset(survey_sh_names, "[D,d]escription")
  if(length(desc_sh)<1){
    stop("The spreadsheet does not include a sheet named 'description'")
  }

  dat_desc <- googlesheets4::read_sheet(survey_url, desc_sh)

  desc_nms <- colnames(dat_desc)

  if(length(desc_nms) > 1){
    dat_desc <- dat_desc %>% rename_with(\(x)stringr::str_replace(x, ".*(?=_..)", "description"))
  } else {
    names(dat_desc) <- paste0("description_", lang)
  }

  pop_file_in <- bind_cols(pop_file_in, dat_desc)

  end <- Sys.time()
  print(end - start)

  # save the file locally so only re-run when asked
  write.csv(pop_file_in, file.path(save_dir, "extdata/temp_pop_file_local.csv"), row.names = FALSE)

  pop_file_in

}
