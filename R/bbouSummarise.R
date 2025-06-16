
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
bbouMakeFigures <- function(surv_fit, recruit_fit, fig_dir, i18n = NULL, ht = 400, wt = 600,
                            show_interpolated = TRUE){
  # make figures
  if(is.null(i18n)){
    i18n <- list(t = function(x)paste0(x))
  }

  ht <- ht*300/72
  wt <- wt*300/72

  #view of the available survival data
  surv_long <- filter(surv_fit$data,!is.na(StartTotal)) %>%
    group_by(PopulationName, Year) %>%
    summarise(StartTotal = max(StartTotal),
              MortalitiesCertain = sum(MortalitiesCertain)) %>%
    pivot_longer(c(StartTotal, MortalitiesCertain), names_to = "Category",
                 values_to = "NumAnimals")

  surv_long$Category[surv_long$Category == "MortalitiesCertain"] <- i18n$t("Deaths")
  surv_long$Category[surv_long$Category == "StartTotal"] <- i18n$t("Collared caribou")
  png(file.path(fig_dir, "survivalSummary.png"),
      height = ht, width = wt, units = "px", res = 300
  )
  base <- ggplot(surv_long, aes(x = as.integer(Year), y = NumAnimals, group = Category,
                                shape = Category, colour = Category)) +
    geom_point(size = 2) +
    ggplot2::facet_wrap(~PopulationName) +
    scale_color_brewer(palette = "Set2", labels = \(x) str_wrap(x, 10))+
    scale_shape_discrete(labels = \(x) str_wrap(x, 10))+
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    scale_x_continuous(minor_breaks = function(lims){ceiling(lims[1]):floor(lims[2])},
                       breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)),
                       guide = guide_axis(minor.ticks = TRUE))+
    labs(x = i18n$t("Year"), y = i18n$t("Number of collared caribou"), colour = i18n$t("Category"),
         shape = i18n$t("Category")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "top")+
    coord_cartesian(clip = 'off')
  print(base)
  dev.off()

  # view of the available recruitment data
  rec_long <- subset(recruit_fit$data, !is.na(Cows), select = c(-Yearlings)) %>%
    mutate(Bulls = CowsBulls - Cows, .after = Cows) %>%
    select(-CowsBulls) %>%
    pivot_longer(Cows:Calves, names_to = "Category", values_to = "NumAnimals") %>%
    mutate(Category = factor(Category, levels = rev(c("Cows", "Calves", "Bulls", "UnknownAdults"))))

  png(file.path(fig_dir, "recruitmentSummary.png"),
      height = ht, width = wt, units = "px", res = 300
  )

  base <- ggplot(rec_long, aes(x = as.factor(Year), y = NumAnimals, group = Category,
                               fill = Category, colour = Category)) +
    geom_bar(position = "stack", stat = "identity") +
    geom_text(aes(label = after_stat(y), group = as.integer(Year)),
              stat = 'summary', fun = sum, vjust = -0.5, show.legend = FALSE)+
    ggforce::facet_row(vars(PopulationName), scale = "free_x", space = "free", strip.position = "left") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
    # scale_x_continuous(minor_breaks = function(lims){ceiling(lims[1]):floor(lims[2])},
    #                    breaks = function(lims){ceiling(lims[1]):floor(lims[2])},
    #                    guide = guide_axis(minor.ticks = TRUE))+
    scale_fill_brewer(palette = "Set2",
                      labels = rev(c(i18n$t('Adult females'), i18n$t('Calves'), i18n$t('Adult males'),
                                       i18n$t('Adults unknown sex'))) %>% stringr::str_wrap(width = 10),
                        aesthetic = c("fill", "colour"))+
    labs(x = i18n$t("Year"), y = i18n$t("Number of caribou counted"),
         fill = i18n$t("Category"), colour = i18n$t("Category")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top")
  print(base)
  dev.off()

  png(file.path(fig_dir,"survBbouMulti.png"), height = ht, width = wt, units = "px", res = 300)
  plt <- bb_plot_year_survival(surv_fit)

  # colour points based on whether the population had data in that year or if it
  # is borrowed from other populations/years
  plt$data <- plt$data %>%
    left_join(surv_fit$data %>%
                mutate(PopulationName, Annual = as.character(Annual) %>%
                         as.numeric(), has_data = 1, .keep = "used"),
              by = c("PopulationName", CaribouYear = "Annual")) %>%
    mutate(has_data = replace_na(has_data, 0) %>% as.character())

  if(!show_interpolated){
    plt$data <- plt$data %>%
      filter(has_data == "1")
  }else {

    mapping <- plt$mapping
    mapping[5][[1]] <- as.name("has_data")
    names(mapping)[5] <- "colour"
    plt$mapping <- mapping

    plt <- plt +
      scale_colour_discrete(type = c("0" = "grey70", "1" = "black"),
                            name = i18n$t("Data from survey"),
                            label = c(i18n$t("No"), i18n$t("Yes")))
  }

  plt2 <- plt +
    labs(x = i18n$t("Year"))+
    scale_x_continuous(breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)))+
    scale_y_continuous(i18n$t("Annual female survival"), labels = scales::percent)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top")
  print(plt2)
  dev.off()

  png(file.path(fig_dir,"recBbouMulti.png"), height = ht, width = wt, units = "px", res = 300)
  plt <- bb_plot_year_recruitment(recruit_fit)

  # colour points based on whether the population had data in that year or if it
  # is borrowed from other populations/years
  plt$data <- plt$data %>%
    left_join(recruit_fit$data %>%
                mutate(PopulationName, Annual = as.character(Annual) %>%
                         as.numeric(), has_data = 1, .keep = "used"),
              by = c("PopulationName", CaribouYear = "Annual")) %>%
    mutate(has_data = replace_na(has_data, 0) %>% as.factor())

  if(!show_interpolated){
    plt$data <- plt$data %>%
      filter(has_data == "1")
  }else {

    mapping <- plt$mapping
    mapping[5][[1]] <- as.name("has_data")
    names(mapping)[5] <- "colour"
    plt$mapping <- mapping

    plt <- plt +
      scale_colour_discrete(type = c("0" = "grey70", "1" = "black"),
                            name = i18n$t("Data from survey"),
                            label = c(i18n$t("No"), i18n$t("Yes")))
  }

  plt2 <- plt +
    scale_y_continuous(transform = scales::new_transform(
      "hundred", transform = function(x){x*100}, inverse = function(x){x/100},
      format = scales::label_number(scale = 100)
    ))+
    scale_x_continuous(breaks = scales::extended_breaks(5, Q = 1:5, w = c(0.25, 0.2, 0.1, 0.5)))+
    labs(x = i18n$t("Year"), y = i18n$t("Calves per 100 females"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top")
  print(plt2)
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
