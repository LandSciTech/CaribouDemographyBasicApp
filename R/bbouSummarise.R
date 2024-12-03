#' Create summary table of demographic rates from survival and recruitment surveys
#'
#' @param surv_data dataframe. Survival data in bboudata format
#' @param recruit_data dataframe. Recruitment data in bboudata format
#' @param N0 dataframe. Initial population estimates, required columns are
#'   PopulationName and N0
#' @param shiny_progress logical. Should shiny progress bar be updated. Only set
#'   to TRUE if using in an app.
#' @param return_mcmc boolean. If TRUE return fitted survival and recruitment
#'   models. Default FALSE.
#' @param ... Other parameters passed on to `bboutools::bb_fit_survival` and
#'   `bboutools::bb_fit_recruitment`.
#'
#' @return If `return_mcmc` is TRUE then a list with results and fitted models,
#'   if FALSE just the results table is returned.
#' @export
#'
#' @examples
#' s_data <- rbind(bboudata::bbousurv_a, bboudata::bbousurv_b)
#' r_data <- rbind(bboudata::bbourecruit_a, bboudata::bbourecruit_b)
#' fitDemographicRates(s_data, r_data, 500, FALSE)

bbouMakeSummaryTable <-function(surv_data, recruit_data, N0, shiny_progress = FALSE,
                                return_mcmc=FALSE,i18n = NULL, ...){
  #shiny_progress = FALSE;return_mcmc=FALSE;i18n = NULL

  if(length(N0)==1){
    N0= expand.grid(PopulationName=unique(surv_data$PopulationName),N0=N0)
  }
  if(is.null(i18n)){
    i18n <- list(t = function(x)paste0(x))
  }

  if(length(unique(surv_data$Year))<5){
    stop("At least 5 years of survival data are needed to estimate interannual variation using bboutools")
  }

  if(length(unique(recruit_data$Year))<5){
    stop("At least 5 years of survival data are needed to estimate interannual variation using bboutools")
  }

  if(shiny_progress) shiny::setProgress(0.2, message = i18n$t("Fitting survival"))

  surv_fit <- bboutools::bb_fit_survival(surv_data, multi_pops = TRUE, allow_missing = TRUE, quiet = TRUE, ...)

  if(shiny_progress) shiny::setProgress(0.4, message = i18n$t("Fitting recruitment"))

  recruit_fit <- bboutools::bb_fit_recruitment(recruit_data, multi_pop = TRUE, allow_missing = TRUE, quiet = TRUE, ...)

  if(shiny_progress) shiny::setProgress(0.6, message = i18n$t("Predicting survival"))
  surv_pred_bar <- bboutools::bb_predict_survival(surv_fit, year = FALSE, month = FALSE, conf_level = FALSE)

  if(shiny_progress) shiny::setProgress(0.8, message = i18n$t("Predicting recruitment"))
  rec_pred_bar <- bboutools::bb_predict_calf_cow_ratio(recruit_fit, year = FALSE, conf_level = FALSE)

  # summarize model output
  data_sur <- surv_pred_bar$data
  data_rec <- rec_pred_bar$data

  # Force matrix b/c if only one it is numeric
  S_samp <- mcmcr::collapse_chains(surv_pred_bar$samples)[, , ] %>% as.matrix()
  R_samp <- mcmcr::collapse_chains(rec_pred_bar$samples)[, , ] %>% as.matrix()
  rownames(S_samp) <- seq(1, nrow(S_samp))
  colnames(S_samp) <- levels(data_sur$PopulationID)
  rownames(R_samp) <- seq(1:nrow(R_samp))
  colnames(R_samp) <- levels(data_rec$PopulationID)

  pops <- levels(data_sur$PopulationID)
  R_Annual <- exp(mean(log(mcmcr::collapse_chains(recruit_fit$samples$sAnnual)[, , ])))
  S_Annual <- exp(mean(log(mcmcr::collapse_chains(surv_fit$samples$sAnnual)[, , ])))
  R_Annual_sd <- sd(log(mcmcr::collapse_chains(recruit_fit$samples$sAnnual)[, , ]))
  S_Annual_sd <- sd(log(mcmcr::collapse_chains(surv_fit$samples$sAnnual)[, , ]))

  for (i in 1:length(pops)) {
    # i=2
    p <- pops[i]
    R_samp_long <- R_samp[, i]
    S_samp_long <- S_samp[, i]
    R_bar <- inv.logit(mean(logit(R_samp_long)))
    R_sd <- sd(logit(R_samp_long))
    R_qt <- quantile(R_samp_long, probs=c(0.025, 0.975))

    S_bar <- inv.logit(mean(logit(S_samp_long)))
    S_sd <- sd(logit(S_samp_long))
    S_qt <- quantile(S_samp_long, probs=c(0.025, 0.975))

    if (i == 1) {
      parTab <- data.frame(
        pop_name = p,
        R_bar = R_bar, R_sd = R_sd, R_iv_cv = R_Annual, R_iv_sd = R_Annual_sd, R_bar_lower = R_qt[1], R_bar_upper = R_qt[2],
        S_bar = S_bar, S_sd = S_sd, S_iv_cv = S_Annual, S_iv_sd = S_Annual_sd, S_bar_lower = S_qt[1], S_bar_upper = S_qt[2]
      )
    } else {
      parTab <- rbind(parTab, data.frame(
        pop_name = p,
        R_bar = R_bar, R_sd = R_sd, R_iv_cv = R_Annual, R_iv_sd = R_Annual_sd, R_bar_lower = R_qt[1], R_bar_upper = R_qt[2],
        S_bar = S_bar, S_sd = S_sd, S_iv_cv = S_Annual, S_iv_sd = S_Annual_sd, S_bar_lower = S_qt[1], S_bar_upper = S_qt[2]
      ))
    }
  }

  # dplyr version, not using but might want to some day...
  # R_samp %>% as_tibble(rownames = "id") %>%
  #   # move pop_name from column name to value
  #   pivot_longer(-id, names_to = "pop_name", values_to = "R") %>%
  #   # do the same to S and add it
  #   full_join(
  #     S_samp %>% as_tibble(rownames = "id") %>%
  #       pivot_longer(-id, names_to = "pop_name", values_to = "S"),
  #     by = c("id", "pop_name")
  #   ) %>%
  #   # calculate mean and sd of recruitment and survival for each pop
  #   group_by(pop_name) %>%
  #   summarise(across(c(R, S), .fns = list(bar = \(x)inv.logit(mean(logit(x))),
  #                                         sd = \(x)sd(logit(x))))) %>%
  #   # add annual columns
  #   mutate(N0 = N0, R_iv_cv = R_Annual, R_iv_sd = R_Annual_sd,
  #          S_iv_cv = S_Annual, S_iv_sd = S_Annual_sd) %>%
  #   # reorder columns
  #   select(pop_name, N0, matches("^R"), matches("^S"))

  # data amount summary
  surv_data_amt <- surv_data %>% group_by(PopulationName, Year) %>%
    summarise(nCollars = max(StartTotal)) %>%
    summarise(nCollarYears = sum(nCollars),
              nSurvYears = n_distinct(Year))

  recruit_data_amt <- recruit_data %>% group_by(PopulationName, Year) %>%
    summarise(nCows = sum(Cows)) %>%
    summarise(nCowsAllYears = sum(nCows),
              nRecruitYears = n_distinct(Year))

  data_amt <- merge(surv_data_amt, recruit_data_amt)

  if(is.element("PopulationName",names(N0))){
    parTab = merge(parTab,N0, by.x = "pop_name", by.y = "PopulationName")
  }else{
    parTab = merge(parTab,N0)
  }

  parTab = merge(parTab, data_amt, by.x = "pop_name", by.y = "PopulationName")

  if(return_mcmc){
    return(list(parTab=parTab,surv_fit=surv_fit,recruit_fit=recruit_fit))
  }else{
    return(parTab)
  }

}

#' Make figures based on fitted survival and recruitment models
#'
#'
#' @param surv_fit result of `bboutools::bb_fit_survival`
#' @param recruit_fit result of `bboutools::bb_fit_recruitment`
#' @param fig_dir file path. Directory where figures should be saved
#' @param i18n shiny.i18n object. Used of translation in Shiny app.
#' @param ht numeric. Height in pixels
#' @param wt numeric. Width in pixels
#'
#' @return Nothing. Figures are saved to `fig_dir`.
#' @export
#'
bbouMakeFigures <- function(surv_fit, recruit_fit, fig_dir, i18n = NULL, ht = 400, wt = 600){
  # make figures
  if(is.null(i18n)){
    i18n <- list(t = function(x)paste0(x))
  }

  #view of the available survival data
  surv_long <- filter(surv_fit$data,!is.na(StartTotal)) %>%
    group_by(PopulationName, Year) %>%
    summarise(StartTotal = max(StartTotal),
              MortalitiesCertain = sum(MortalitiesCertain)) %>%
    pivot_longer(c(StartTotal, MortalitiesCertain), names_to = "Category",
                 values_to = "NumAnimals")

  surv_long$Category[surv_long$Category == "MortalitiesCertain"] <- i18n$t("Mortalities")
  surv_long$Category[surv_long$Category == "StartTotal"] <- i18n$t("CollarYrs")
  png(paste0(fig_dir, "/survivalSummary.png"),
      height = ht, width = wt, units = "px"
  )
  base <- ggplot(surv_long, aes(x = as.integer(Year), y = NumAnimals, group = Category,
                                shape = Category, colour = Category)) +
    geom_point() +
    facet_wrap(~PopulationName) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    scale_x_continuous(minor_breaks = function(lims){ceiling(lims[1]):floor(lims[2])},
                       breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)),
                       guide = guide_axis(minor.ticks = TRUE))+
    labs(x = i18n$t("Year"), y = i18n$t("Number of animals"), colour = i18n$t("Category"),
         shape = i18n$t("Category")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(base)
  dev.off()

  # view of the available recruitment data
  rec_long <- subset(recruit_fit$data, !is.na(Cows), select = c(-Yearlings)) %>%
    pivot_longer(Cows:Calves, names_to = "Category", values_to = "NumAnimals")

  png(paste0(fig_dir, "/recruitmentSummary.png"),
      height = ht, width = wt, units = "px"
  )

  # included so they are added to translate file
  c(i18n$t('Cows'), i18n$t('CowsBulls'), i18n$t('UnknownAdults'), i18n$t('Calves'))

  base <- ggplot(rec_long, aes(x = as.integer(Year), y = NumAnimals, group = Category,
                               fill = Category, colour = Category)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(~PopulationName) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    scale_x_continuous(minor_breaks = function(lims){ceiling(lims[1]):floor(lims[2])},
                       breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)),
                       guide = guide_axis(minor.ticks = TRUE))+
    scale_fill_discrete(labels = i18n$t, aesthetic = c("fill", "colour"))+
    ylab("Number of animals") +
    labs(x = i18n$t("Year"), y = i18n$t("Number of animals"),
         fill = i18n$t("Category"), colour = i18n$t("Category")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(base)
  dev.off()

  png(paste0(fig_dir,"/survBbouMulti.png"), height = ht, width = wt, units = "px")
  plt <- bb_plot_year_survival(surv_fit)+
    labs(x = i18n$t("Year"), y = i18n$t("Annual Survival (%)"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(plt)
  dev.off()

  png(paste0(fig_dir,"/recBbouMulti.png"), height = ht, width = wt, units = "px")
  plt <- bb_plot_year_recruitment(recruit_fit)+
    scale_y_continuous(transform = scales::new_transform(
      "hundred", transform = function(x){x*100}, inverse = function(x){x/100},
      format = scales::label_number(scale = 100)
    ))+
    scale_x_continuous(breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)))+
    labs(x = i18n$t("Year"), y = i18n$t("Calves per 100 cows"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  print(plt)
  dev.off()

  # Not sure if we should use these since haven't used the lambda concept in the app so far
  # predict_lambda <- bb_predict_growth(survival = surv_fit, recruitment = recruit_fit)
  # png(paste0(fig_dir,"/lambdaBbouMulti.png"),
  #     height = ht, width = wt, units = "px",res=600)
  # plt <- bb_plot_year_growth(predict_lambda) +
  #   scale_y_continuous(labels = scales::percent)+
  #   labs(x = i18n$t("Year"), y = i18n$t("Population growth (lambda)"))
  # print(plt)
  # dev.off()

  # predict_change <- bb_predict_population_change(survival = surv_fit, recruitment = recruit_fit)
  # bb_plot_year_population_change(predict_change)
}
