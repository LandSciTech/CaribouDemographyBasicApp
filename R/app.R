# Run to install dependencies
# devtools::install_deps()

#' Run the Boreal Caribou Demographic Projection Explorer
#'
#' @param private If private then you will need to follow instructions in console to authenticate google drive
#'
#' @return launches the app in your default browser
#' @export
#'
run_caribou_demog_app <- function(private = FALSE, lang = "en"){

  inst_dir <- system.file(package = "CaribouDemographyBasicApp")

  #Authenticate Google Sheets
  if(!private){
    # # note it must be saved as a google sheet. Saving as excel in Google does not work
    def_survey_url <- "https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing"

  } else {
    # Quebec survey url
    def_survey_url <- "https://docs.google.com/spreadsheets/d/1FJZ06dc1-oKUEsNrrjSfqlEn3MGsXODQhJ3gmbqmp-s/edit?usp=sharing"

  }


  theme_set(
    theme_classic()+
      theme(text = element_text(size = 20),
            plot.caption = element_text(hjust = 0))
  )
  # File with translations
  i18n <- Translator$new(translation_csvs_path = file.path(inst_dir, "extdata/translations"))
  i18n$set_translation_language(lang) # default translation to display

  # add JavaScript to add an id to the <section> tag
  # so we can overlay waiter on top of it
  # Also creates an input with the window width for use in plot sizing
  add_id_to_section <- "
$( document ).ready(function() {
  var section = document.getElementsByClassName('content');
  section[0].setAttribute('id', 'waiter-content');
});
var dimension = [0, 0];
$(document).on('shiny:connected', function(e) {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange('dimension', dimension);
});
$(window).resize(function(e) {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange('dimension', dimension);
});
"

  mod_defaults <- caribouPopGrowth %>% formals() %>% eval()

  pop_file <- read.csv(file.path(inst_dir, "extdata/temp_pop_file_local.csv"))

  # UI #-------------------------------------------------------------------------
  {
    ui <-  page_sidebar(
      window_title = "app",
      padding = 1,
      div(class = "container-fluid", style ="background-color: #E8BA59",
          span(i18n$t("Boreal Caribou Demographic Projection Explorer"),
             style = "color: white; font-size: 30px; font-weight: bold")
      ),
      theme = bs_theme(bootswatch = "bootstrap", success = "#E8BA59") %>%
        bs_add_rules(
          list(
            "body { --bslib-page-sidebar-title-bg: #E8BA59; }"
          )
        ),

      # Sidebar --------------------------------------------------------------------
      sidebar = sidebar(
        shiny.i18n::usei18n(i18n),
        shinyjs::useShinyjs(),
        width = 400,
        open = "always",
        uiOutput("lang_select_ui"),
        h4(i18n$t("Define simulation inputs")),
        uiOutput("param_source_ui"),
        numericInput("numSteps",
                     label = i18n$t("Number of years"),
                     value = 20
        ) %>% tagAppendAttributes(style = 'font-size:20px'),
        # Alternative Scenarios #-------------------------------------------------

        h4(i18n$t("Run model"), id = "run-section"),
        actionButton("run_model", i18n$t("Run model"), icon("paper-plane"),
                     class = "btn-success"),
        actionButton("add_alt", i18n$t("Add alternative scenario"),
                     icon("plus", lib = "glyphicon"),
                     class = "btn-success"),
        actionButton("update_data", i18n$t("Update data"),
                     icon("cloud-download", lib = "glyphicon"),
                     class = "btn-success"),

        uiOutput("adv_params_ui")

        # This .rmd file is missing
        # radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word'),
        #              selected="HTML",  inline = TRUE),
        # downloadButton("downloadOutput", label = "Download output"),
      ),
      # Body #---------------------------------------------------------------------
      # import our custom JavaScript
      tags$head(
        tags$script(add_id_to_section)
      ),
      waiter::useWaiter(),
      navset_bar(
        id = "body",
        fillable = FALSE,
        nav_panel(
          i18n$t("Welcome"),
          value = "welcome_tab",
          uiOutput("welcome")
        ),
        nav_panel(
          i18n$t("Survey data"),
          value = "input_data_tab",
          uiOutput("input_data")
        ),
        nav_panel(
          i18n$t("Projections"),
          value = "results_tab",
          uiOutput("results")
        ),
        nav_panel(
          i18n$t("Documentation"),
          uiOutput("documentation")
        )
      )
    )
  }

  # Server #----------------------------------------------------------------------
  server <- function(input, output, session) {

    output$lang_select_ui <- renderUI({
      input$selected_language
      # languages tbl
      lang_choice <- data.frame(
        language = c("English", "Français"),
        code = c("en", "fr")
      )
      lang_choice <- lang_choice$code %>% set_names(lang_choice$language)

      if(isTruthy(input$selected_language)){
        selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = lang_choice,
                    selected = input$selected_language) %>%
          tagAppendAttributes(style = 'font-size:20px')
      }else {
        selectInput('selected_language',
                    i18n$t("Change language"),
                    choices = lang_choice,
                    selected = lang) %>%
          tagAppendAttributes(style = 'font-size:20px')
      }

    })


    observeEvent(input$selected_language, {
      # This print is just for demonstration
      print(paste("Language change!", input$selected_language))
      # Here is where we update language in session
      shiny.i18n::update_lang(input$selected_language)
    })

    # make model run on start up
    o <- observe({
      req(pop_file())
      req(input$S_bar)
      shinyjs::click("run_model")
      nav_select(id = "body", selected = "welcome_tab")
      o$destroy() # destroy observer as it has no use after initial button click
    })

    output$param_source_ui <- renderUI({
      input$selected_language

      pops_use <- pop_file() %>% filter(nSurvYears > 1, nRecruitYears > 1) %>%
        pull(pop_name)

      tagList(
        # selectInput(
        #   "param_source",
        #   label = i18n$t("Create population parameters from..."),
        #   choices = c("file","man") %>%
        #     set_names(c(i18n$t("Existing data"),
        #                 i18n$t("Manual parameter entry"))),
        #   selected = "file"
        # ),
        div(
          id = "from_file",
          selectInput("pop_name", label = i18n$t("Population name"),
                      choices = pops_use
          ) %>% tagAppendAttributes(style = 'font-size:20px')
        )
      )
    })
    output$adv_params_ui <- renderUI({
      input$selected_language
      # rendering dynamically so translation works
      accordion(
        open = FALSE,
        accordion_panel(
          i18n$t("Other model parameters"),
          value = "adv_mod_params",
          markdown(paste0(i18n$t("Advanced users can adjust these parameters to explore sensitivity to various demographic model assumptions."), " ",
                          i18n$t("Detailed descriptions will be provided in the app documentation (not yet written).")," ",
                          i18n$t("In the meantime, see the"), " ",
                          "[caribouMetrics](https://landscitech.github.io/caribouMetrics/articles/caribouDemography.html)",
                          " ",
                          i18n$t("package documentation to learn about some of the parameters."))),
          numericInput("numPops",
                       label = i18n$t("Number of example trajectories"),
                       value = 35
          ),
          uiOutput("cur_pop_ui")
        )
      )
    })

    # make reactive so it can be updated
    pop_file <- reactiveVal(pop_file)

    # Current population --------------------------------
    output$cur_pop_ui <- renderUI({
      # req(input$param_source)
      # if(input$param_source == "file"){
      # option for loading data from file
      input$selected_language
      req(input$pop_name)
      pop_default <- pop_file() %>% filter(pop_name == input$pop_name) %>%
        select(-pop_name, -contains("iv"))

      iv_default <- pop_file() %>% filter(pop_name == input$pop_name) %>%
        select(contains("iv")) %>%
        rename_with(\(x)str_remove(x, "_iv") %>% str_to_upper())

      iv_default$type <- "logistic"
      # } else {
      #   iv_default <- eval(formals(caribouPopGrowth)$interannualVar)
      #   iv_default$type <- "beta"
      #   iv_default$R_SD <- 0
      #   iv_default$S_SD <- 0
      #
      #   pop_default <- demographicRates(data.frame(fire = 0, Anthro = 0, fire_excl_anthro = 0),
      #                                   demographicCoefficients(100)) %>%
      #     mutate(across(everything(), \(x)round(x, 2)),
      #            N0 = 1000) %>%
      #     rename_with(\(x)str_replace(x, "stdErr", "sd"))
      # }
      accordion(
        open = FALSE,
        id = "cur_pop",
        accordion_panel(
          title = i18n$t("Estimated demographic parameters"),
          div(
            id = "manual_entry",
            selectInput("ivType",
                        label = i18n$t("Type of interannual variation"),
                        choices = c("beta",
                                    "logistic"),
                        selected = iv_default$type
            ),
            numericInput("N0",
                         label = i18n$t("Initial number of adult females"),
                         value = pop_default$N0, min = 0
            ),
            sliderInput("S_bar",
                        label = i18n$t("Average % female survival"),
                        value = pop_default$S_bar * 100, min = 0, max = 99, step = 0.1
            ),
            sliderInput("R_bar",
                        label = i18n$t("Average calves per 100 females"),
                        value = pop_default$R_bar * 100, min = 0, max = 100, step = 0.1
            ),
            numericInput("S_sd",
                         label = i18n$t("Standard deviation of survival rate"),
                         value = round(pop_default$S_sd, 3)
            ),
            numericInput("R_sd",
                         label = i18n$t("Standard deviation of calves per 100 females"),
                         value = round(pop_default$R_sd, 3)
            ),
            sliderInput("S_iv_cv",
                        label = i18n$t("Interannual variation of survival rate"),
                        value = iv_default$S_CV, min = 0, max = 2, step = 0.01
            ),
            sliderInput("R_iv_cv",
                        label = i18n$t("Interannual variation of calves per 100 females"),
                        value = iv_default$R_CV, min = 0, max = 2, step = 0.01
            ),
            sliderInput("S_iv_sd",
                        label = i18n$t("Uncertainty about interannual variation of survival rate"),
                        value = iv_default$S_SD, min = 0, max = 1.5,step=0.01
            ),
            sliderInput("R_iv_sd",
                        label = i18n$t("Uncertainty about interannual variation of calves per 100 females"),
                        value = iv_default$R_SD, min = 0, max = 1.5,step=0.01
            )
          )
        ),
        addl_params_ui("cur", mod_defaults, i18n, i18n$t("Current"))
      )

    })

    outputOptions(output, "cur_pop_ui", suspendWhenHidden = FALSE)

    # observeEvent(input$param_source, {
    #   if(input$param_source == "man"){
    #     accordion_panel_open("cur_pop", values = TRUE)
    #   }
    #   shinyjs::toggle("from_file", condition = input$param_source == "file")
    # })

    # # Alt scenario UI #-----------------------------------------------------------
    counter <- reactiveValues(n = 0)

    observeEvent({
      input$add_alt
    }, {
      req(input$add_alt)
      counter$n <- counter$n + 1

      n <- counter$n
      r <- input$R_bar
      s <- input$S_bar

      insertUI(
        div(id = paste0("alt_menu_", n),
            accordion(
              id = paste0("alt_box_", n),
              open = TRUE,
              accordion_panel(
                title = paste0(i18n$t("Alternative scenario"), " ", n),
                value = paste0("alt_box_p_", n),
                textInput(inputId = paste0("alt_name_", n),
                          label = i18n$t("Alternative scenario name")),
                sliderInput(inputId = paste0("alt_S_bar_", n),
                            label = i18n$t("Average % female survival"),
                            value = input$S_bar, min = 0, max = 99, step = 1),
                sliderInput(inputId = paste0("alt_R_bar_", n),
                            label = i18n$t("Average calves per 100 females"),
                            value = input$R_bar, min = 0, max = 100, step = 1),
                actionButton(paste0("alt_remove_", n),
                             i18n$t("Remove scenario"), icon("remove", lib = "glyphicon"),
                             style = "color: #fff; background-color: #800000; border-color: #800000")
              )
            )
        ),
        selector = "#run-section",
        where = "beforeBegin"
      )

      insertUI(addl_params_ui(n, mod_defaults, i18n, paste0(i18n$t("Scenario"), " ", n)),
               selector = "#cur_pop",
               where = "afterEnd")

      # close any existing alt scenario boxes
      to_close <- get_not_null_input("alt_box", input) %>% names()
      walk(to_close, \(x){
        accordion_panel_close(x, values = TRUE)
      })
    })


    remove_lst <- reactive({
      out <- get_not_null_input("alt_remove_", input)

      if(length(out) == 0){
        req(FALSE)
      }
      if(!any(unlist(out) > 0)){
        req(FALSE)
      }
      out
    })

    observeEvent(remove_lst(), {
      to_remove <- names(which((remove_lst() %>% unlist()) > 0)) %>%
        str_remove("alt_remove")

      removeUI(selector = paste0("div[id*='alt_menu", to_remove, "']"))

      removeUI(selector = paste0("div[id*='addl_params", to_remove, "']"))

      in_to_remove <- str_subset(names(input), paste0("^alt_.*", to_remove))

      # Note this sets the input to NULL but it still exists and has a name in
      # input
      walk(in_to_remove, \(x){
        shinyjs::runjs(paste0('Shiny.onInputChange("',x,'", null)'))
      })
    })

    #
    # # Run simulations #---------------------------------------------------------
    do_alt_scns <- reactive({
      alt_nms <-  str_subset(names(input), "^alt_")
      out <- map(alt_nms, \(x)input[[x]]) %>% set_names(alt_nms)

      if(length(out) == 0){
        return(FALSE)
      }

      unlist(out) > 0
    })


    pop_mod <- eventReactive(input$run_model, {

      cur_res <- suppressMessages(doSim(max(c(input$numSteps, 100)), input$numPops, N0 = input$N0,
                                        R_bar = input$R_bar/100, S_bar = input$S_bar/100,
                                        R_sd = input$R_sd, S_sd = input$S_sd,
                                        R_iv_cv = input$R_iv_cv, S_iv_cv = input$S_iv_cv,
                                        R_iv_sd = input$R_iv_sd, S_iv_sd = input$S_iv_sd,
                                        scn_nm = "Current", type = input$ivType,
                                        addl_params = get_addl_params("cur", input)))

      if(any(do_alt_scns())){
        R_lst <- get_not_null_input("alt_R_bar_", input)
        S_lst <- get_not_null_input("alt_S_bar_", input)
        scn_nms_lst <- get_not_null_input("alt_name_", input)

        scn_res <- pmap_dfr(list(R_lst, S_lst, scn_nms_lst, names(scn_nms_lst)),
                            \(x, y, z, nm){
                              suppressMessages(doSim(max(c(input$numSteps, 100)), input$numPops, N0 = input$N0,
                                    R_bar = x/100,
                                    S_bar = y/100,
                                    R_sd = input$R_sd, S_sd = input$S_sd,
                                    R_iv_cv = input$R_iv_cv, S_iv_cv = input$S_iv_cv,
                                    R_iv_sd = input$R_iv_sd, S_iv_sd = input$S_iv_sd,
                                    scn_nm = ifelse(z == "", nm, z),
                                    type = input$ivType,
                                    addl_params = get_addl_params(str_remove(nm, "alt_name_"),
                                                                  input)))
                            })
      }else {
        scn_res <- NULL
      }



      out <- bind_rows(cur_res, scn_res)

      # set factor levels so Current is always first
      scn_levs <- c("Current", setdiff(unique(out$scn), "Current"))
      scn_labs <- c("Current", setdiff(unique(out$scn), "Current"))

      out <- out %>%
        mutate(scn = factor(scn, levels = scn_levs, labels = scn_labs))

      return(out)

    })
    # Population plot #--------------------------------------------------------------
    output$pop_plot <- renderPlot({
      req(pop_mod())

      pop_plt <- pop_mod() %>%
        filter(time <= isolate(input$numSteps))

      plt <- pop_plt %>%
        bind_rows(
          distinct(pop_mod(), type, scn, id, time = 0, N = isolate(input$N0))
        ) %>%
        mutate(type = factor(type, levels = c("samp", "mean")),
               time = as.factor(time)
        ) %>%
        ggplot(
          aes(x = time, y = N, group = interaction(id, scn, type),
              linewidth = type, colour = scn, alpha = type))+
        geom_line()+
        scale_y_continuous(limits = c(0, min(input$N0*3, max(pop_plt$N))), expand = expansion())+
        scale_x_discrete(expand = expansion())+
        scale_linewidth_discrete(range = c(1, 2),
                                 breaks = c("samp", "mean"),
                                 labels = c(i18n$t("Possible"), i18n$t("Mean")))+
        scale_alpha_discrete(range = c(0.2, 1), guide = NULL)+
        scale_color_brewer(palette = "Dark2", labels = \(x)i18n$t(x))+
        # scale_colour_identity(guide = guide_legend(), labels = c()) +
        labs(col = i18n$t("Scenario"), y = i18n$t("Female population size") %>% str_wrap(25),
             x = i18n$t("Years in the future"),
             alpha = i18n$t("Trajectory"), linewidth = i18n$t("Trajectory"),
             caption = i18n$t("Projected adult female population size over time. The darker line shows the outcome if we ignore uncertainty about demographic rates and variation among years. The paler lines show a variety of plausible outcomes given uncertainty about demographic rates and variation among years. Only the female population is shown because it is assumed that the number of females is what limits population growth. The population is considered stable if the line is flat or sloped upwards and is declining if the line slopes downward.") %>%
               str_wrap(max(pop_cont_w(), 700)/7.5 - 10))+
        guides(linewidth = guide_legend(override.aes = list(alpha = c(0.2, 1))),
               colour = guide_legend(override.aes = list(linewidth = c(2))))

      plt
    })

    # Population change plot #---------------------------------------------------
    output$pop_change <- renderPlot({
      req(pop_mod())
      pop_plt <- pop_mod() %>%
        filter(time <= isolate(input$numSteps))

      ddg <- position_dodge()

      pop_plt %>%
        mutate(deaths = N0 - surviving_adFemales,
               fem_recruit = n_recruits/2,
               time = as.factor(time)) %>%
        pivot_longer(c(deaths, fem_recruit), names_to = "var", values_to = "num_animals") %>%
        pivot_wider(names_from = type, values_from = num_animals) %>%
        group_by(time, scn, var) %>%
        summarise(num_animals = mean(mean, na.rm = TRUE),
                  min_animals = min(samp, na.rm = TRUE),
                  max_animals = max(samp, na.rm = TRUE),
                  .groups = "drop") %>%
        ggplot(aes(time, y = num_animals, ymax = max_animals, ymin = min_animals,
                   fill = scn, alpha = var))+
        geom_col(position = ddg)+
        geom_errorbar(position = ddg)+
        scale_fill_brewer(palette = "Dark2")+
        scale_y_continuous(expand = expansion())+
        scale_x_discrete(expand = expansion())+
        scale_alpha_discrete(range = c(0.5, 1), labels = c(i18n$t("Deaths"), i18n$t("Recruits")))+
        labs(fill = i18n$t("Scenario"), y = i18n$t("Number of females"),
             x = i18n$t("Years in the future"), alpha = NULL)+
        facet_wrap(~scn, ncol = 1)


    })

    observeEvent(input$run_model, {
      if(input$run_model > 1){
        nav_select(id = "body", selected = "results_tab")
      }
    })


    # Population stats table #---------------------------------------------------
    pop_table <- eventReactive(input$run_model,{

      cur_tab <- data.frame(Scenario = "Current",
                        R_t_mean = input$R_bar,
                        S_t_mean = input$S_bar)

      if(any(do_alt_scns())){
        R_lst <- get_not_null_input("alt_R_bar_", input)
        S_lst <- get_not_null_input("alt_S_bar_", input)
        scn_nms_lst <- get_not_null_input("alt_name_", input)

        alt_tab <- pmap_dfr(list(R_lst, S_lst, scn_nms_lst, names(scn_nms_lst)),
                            \(x, y, z, nm) data.frame(
                              Scenario = ifelse(z == "", nm, z),
                              R_t_mean = x,
                              S_t_mean = y
                            ))
      } else {
        alt_tab <- NULL
      }

      in_tab <- bind_rows(cur_tab, alt_tab)

      min_max_tab <- pop_mod() %>%
        filter(type == "samp", time <= isolate(input$numSteps)) %>%
        group_by(scn, id) %>%
        summarise(across(matches("[R,S]_t"), mean), .groups = "drop_last") %>%
        summarise(across(matches("[R,S]_t"), list(min = min, max = max)))

      yr_min_pop <- pop_mod() %>% filter(type == "mean") %>% group_by(scn) %>%
        summarise(
          `Time to < 10 females` =
            ifelse(length(which(N < 10)) == 0, paste0(">", max(c(input$numSteps, 100))),
                   as.character(time[min(which(N < 10))]))
        )

      left_join(in_tab, yr_min_pop, by = join_by(Scenario == scn)) %>%
        left_join(min_max_tab, by = join_by(Scenario == scn))
    })

    output$pop_table <- renderTable({
      bar_bounds <- pop_file() %>% filter(pop_name == input$pop_name) %>%
        select(matches("upper|lower")) %>%
        mutate(across(everything(), \(x){round(x * 100)}))

      pct_change <- function(old, new, digits = 0){
        out <- round((new - old)/old *100, digits)
        paste0(ifelse(out > 0, "+", ""), out)
      }

      pop_table() %>%
        mutate(
          `Calves per 100 females` = ifelse(
            Scenario == "Current",
            paste0(round(input$R_bar, 0), "<br>", i18n$t("Range"), ": ",
                   bar_bounds$R_bar_lower, "-", bar_bounds$R_bar_upper),
            paste0(round(R_t_mean, 0),  "<br>% ",i18n$t("Change"),": ",
                   pct_change(input$R_bar, R_t_mean))
          ),
          `Female survival` = ifelse(
            Scenario == "Current",
            paste0(round(input$S_bar, 0), "<br>", i18n$t("Range"), ": ",
                   bar_bounds$S_bar_lower, "-", bar_bounds$S_bar_upper, "%"),
            paste0(round(S_t_mean, 0), "%<br>% ",i18n$t("Change"),": ",
                   pct_change(input$S_bar, S_t_mean))
          ), .before = `Time to < 10 females`
        ) %>%
        select(-R_t_mean, -S_t_mean, -matches("._t_m")) %>%
        # rename_with(\(x){paste(ifelse(str_detect(x, "min"), "Minimum", "Maximum"),
        #                        "realized",
        #                        ifelse(str_detect(x, "^R"), "recruitment", "survival"))},
        #             .cols = matches("._t_m"))%>%
        mutate(Scenario = ifelse(Scenario == "Current", i18n$t("Current"), Scenario)) %>%
        set_names(c(i18n$t("Scenario"), i18n$t("Calves per 100 females"), i18n$t("Female survival"),
                    i18n$t("Years to < 10 females")))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 0,
    sanitize.text.function = identity)

    # Recruitment and mortality plot #---------------------------------------------------
    # Only on button click so don't rerun simulations on resize window
    pop_cont_w <- eventReactive(input$run_model,{
      if(!is.null(session$clientData[["output_pop_plot_width"]])){
        return(session$clientData[["output_pop_plot_width"]])
      }else {
        # Uses window size if container does not exist yet
        return(input$dimension[1]-450)
      }
    })

    # Only on button click so don't rerun simulations on resize window
    r_m_cont_w <- eventReactive(input$run_model,{
      session$clientData[["output_r_m_plot_width"]]
    })

    output$r_m_plot <- renderPlot({
      req(pop_table())

      pop_table() %>%
        mutate(Scenario = factor(Scenario,
                                 levels = c("Current",
                                            setdiff(unique(pop_table()$Scenario),
                                                    "Current")),
                                 labels = c(i18n$t("Current"),
                                            setdiff(unique(pop_table()$Scenario),
                                                    "Current"))),
               across(matches("min|max"), ~.x*100)) %>%
        select(Scenario, matches("[R,S]_t_")) %>%
        pivot_longer(matches("[R,S]_t_"), names_to = c(".value","var"),
                     names_sep = "_t_") %>%
        mutate(B = R*S/200,
               M = (100 - S), .keep = "unused") %>%
        pivot_wider(names_from = var, values_from = c(B, M)) %>%
        pivot_longer(-Scenario, names_to = c("var", ".value"), names_sep = "_") %>%
        mutate(var = ifelse(var == "B", i18n$t("Female replacement rate"),
                            i18n$t("% Female mortality")) %>% str_wrap(14)) %>%

        ggplot(aes(var, y = mean, ymax = max, ymin = min, fill = Scenario))+
        geom_col()+
        geom_errorbar()+
        facet_wrap(~Scenario, nrow = 1)+
        scale_fill_brewer(palette = "Dark2")+
        labs(x = NULL, y = NULL, fill = i18n$t("Scenario"),
             caption = i18n$t("The mean % female mortality and female replacement rate in each scenario. The error bars show the minimum and maximum expected values given the uncertainty in the population parameters. A population is stable or increasing when the female replacement rate is equal to or greater than % female mortality.") %>%
               str_wrap(min(100+(250*(n_distinct(pop_mod()$scn))), r_m_cont_w())/6 - 10))
    })

    # Body UI #-------------------------------------------------------------------
    output$welcome <- renderUI({
      req(input$selected_language)
      page_fillable(
        includeMarkdown(file.path(inst_dir, "app_text",
                                   paste0("intro_", input$selected_language, ".md"))),
        # this hidden input allows us to wait for this UI to render before adding to it
        div(style = "display:none", textInput(inputId = "hidden", label = "", value = "1"))
      )
    })
    output$input_data <- renderUI({
      pop_file()
      page_fillable(
        layout_column_wrap(
          width = "600px",
          heights_equal = "row",
          card(
            full_screen = TRUE,
            h4(i18n$t("Data description"), id = "intro-data-descrip"),
            markdown(pop_file()$description[1]),
            max_height = 300
          ),
          card(
            h4(i18n$t("Disclaimer"), id = "intro-survey-data"),
            p(i18n$t('Note that the data summarized and described here is provided by app users, and will differ among projects. The creators of the app do not take responsibility for the quality of the data. Click the “update data” button on the menu to the left and follow instructions to modify the data or data description.')),
            max_height = 300
          ),
          navset_card_tab(
            title = h4(i18n$t("Survey data summary")),
            nav_panel(
              i18n$t("Survival"),
              card_image(file = file.path(inst_dir, "www", "survivalSummary.png"),
                         fill = FALSE, width = 600),
              height = 400
            ),
            nav_panel(
              i18n$t("Recruitment"),
              card_image(file = file.path(inst_dir, "www", "recruitmentSummary.png"),
                         fill = FALSE, width = 600),
              height = 400
            )),
          navset_card_tab(
            title = h4(i18n$t("Demographic rate estimates")),
            nav_panel(
              i18n$t("Survival"),
              card_image(file = file.path(inst_dir, "www", "survBbouMulti.png"),
                         fill = FALSE, width = 600),
              height = 400
            ),
            nav_panel(
              i18n$t("Recruitment"),
              card_image(file = file.path(inst_dir, "www", "recBbouMulti.png"),
                         fill = FALSE, width = 600),
              height = 400
            )
          ),
          card(
            h4(i18n$t("Data summary"), id = "intro-data-summary"),
            tableOutput("data_summary"),
            p(em(i18n$t("Note that results will not be shown for populations with only one year of recruitment or survival data because one year of data is insufficient for making reliable predictions.")))
          )
        )
      )
    })
    output$results <- renderUI({
      page_fillable(
        layout_columns(
          col_widths = c(12, 6, 3, 3, 12),

          navset_tab(
            nav_panel(i18n$t("Female population"),
                      plotOutput("pop_plot")),
            # nav_panel(i18n$t("Female population change"),
            #           plotOutput("pop_change"))
          ),

          tableOutput("pop_table"),
          card(
            full_screen = TRUE,
            card_header(i18n$t("Glossary")),
            includeMarkdown(file.path(inst_dir, "app_text",
                                       paste0("glossary_", input$selected_language, ".md"))),
            height = "150px"
          ),
          card(
            full_screen = TRUE,
            card_header(i18n$t("Frequently asked questions")),
            includeMarkdown(file.path(inst_dir, "app_text",
                                      paste0("faq_", input$selected_language, ".md"))),
            height = "150px"
          ),
          plotOutput("r_m_plot", fill = FALSE,
                     width = min(100+(350*(n_distinct(pop_mod()$scn))), max(400, pop_cont_w())))
        )
      )
    })
    output$documentation <- renderUI({
      withMathJax(
        includeMarkdown(
          #TODO
          ".md File to be written"
        )
      )
    })

    output$data_summary <- renderTable({
      pop_mod()
      pop_file_in <- pop_file()

      pop_file_in %>%
        select(pop_name, N0, Year,
               # nCollarYears,
               nSurvYears,
               # nCowsAllYears,
               nRecruitYears) %>%
        set_names(c(i18n$t("Population name"),
                    i18n$t("Initial population"),
                    i18n$t("Initial population year"),
                    # i18n$t("Total number of collars by year"),
                    i18n$t("Years of survival data"),
                    # i18n$t("Total number of females in aerial surveys"),
                    i18n$t("Years of recruitment data")))

    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 0)

    outputOptions(output, "pop_plot", suspendWhenHidden = FALSE)
    outputOptions(output, "r_m_plot", suspendWhenHidden = FALSE)
    # Update data #---------------------------------------------------------------
    observeEvent(
      input$update_data,
      {
        showModal(modalDialog(
          fluidPage(
            h3(i18n$t("Update survey data")),
            p(i18n$t("Provide a URL linking to a Google Drive spreadsheet")),
            textInput("survey_url",
                      i18n$t("Google Drive spreadsheet URL:"),
                      value = def_survey_url),
            accordion(
              accordion_panel(
                title = i18n$t("Instructions to create a new data sheet"),
                p(i18n$t("You can supply a URL for your own Google Sheet to use your own data in the app.")),
                tags$ul(
                  tags$li(i18n$t("In your Google Drive account click 'New' and select 'Google Sheets'")),
                  tags$li(i18n$t("Fill the spreadsheet with your data. It must have the same sheets, and column names as the")," ",
                          a(i18n$t("example sheet."), href = "https://docs.google.com/spreadsheets/d/1i53nQrJXgrq3B6jO0ATHhSIbibtLq5TmmFL-PxGQNm8/edit?usp=sharing", target="_blank"), " ",
                          i18n$t("See the")," ",
                          a(i18n$t("bboutools website"), href = "https://poissonconsulting.github.io/bboutools/articles/bboutools.html#providing-data", target="_blank"), " ",
                          i18n$t("for more information on how to format the data.")),
                  tags$li(i18n$t("Note: The data must be in a Google Sheet not an Excel file stored on Google Drive. To use an uploaded Excel file click 'File', then 'Save as Google Sheets'.")),
                  tags$li(i18n$t("If access to the Google Sheet is restricted it will be necessary to grant access to the app.")),
                  tags$li(i18n$t("The simpler option is to click 'Share' and under General Access select 'Anyone with the link` and choose the role 'Viewer'.")),
                  tags$li(i18n$t("Then click 'Copy link'. Paste the link into the text box above and click `Update`"))
                )
              )
            )
          ),
          footer = tagList(
            modalButton(i18n$t("Cancel")),
            actionButton("update_data_submit", i18n$t("Update"))
          ),
          size = "l"
        ))
      }
    )

    do_update <- reactiveVal(FALSE)

    observeEvent(
      input$update_data_submit, {
        # this lets Google sheets work for a public sheet without authentication
        googlesheets4::gs4_deauth()

        sh_name <- tryCatch(
          googlesheets4::gs4_get(input$survey_url)$name,
          error = function(e) e
        )

        if(!inherits(sh_name, "error")){
          do_update(TRUE)
        } else if(sh_name$message == "Client error: (403) PERMISSION_DENIED"){
          showModal(modalDialog(
            i18n$t("The url provided is for a protected Google sheet."),
            i18n$t("Click the Authenticate button below and follow the instructions to give this app permission to access files in your Google drive."),
            i18n$t('Alternatively, change the settings on the Google sheet so "Anyone with the link can view"'),
            footer = tagList(
              modalButton(i18n$t("Cancel")),
              actionButton("auth_gs", i18n$t("Authenticate"))
            ),
            size = "l"))
        } else {
          stop("Unrecognized error accessing spreadsheet")
        }


      }
    )

    observeEvent(input$auth_gs, {
      #Authenticate Google Sheets
      googlesheets4::gs4_auth(email = TRUE)
      do_update(TRUE)
    })

    observeEvent(
      do_update(), {
        req(do_update())

        showModal(modalDialog(
          i18n$t("Please wait while the data is processed. This may take several minutes."),
          i18n$t("See the progress bar in the bottom right."),
          footer = NULL,
          size = "m"))

        start <- Sys.time()
        withProgress({
          sh_name <- googlesheets4::gs4_get(input$survey_url)$name

          shiny::setProgress(0.1, message = paste0(i18n$t("Downloading data from "), sh_name))
          survey_sh_names <- googlesheets4::sheet_names(input$survey_url)

          recruit_sh <- stringr::str_subset(survey_sh_names, "[R,r]ecruit")
          if(length(recruit_sh)<1){
            stop("The spreadsheet does not include a sheet named recruit")
          }
          survey_recruit <- googlesheets4::read_sheet(input$survey_url, recruit_sh,
                                                      na = "NA") %>%
            filter(if_all(everything(), \(x)!is.na(x))) %>%
            bboudata::bbd_chk_data_recruitment(multi_pops = TRUE)

          # Error in make bbouSummary table if only 1 year
          #survey_recruit <- survey_recruit %>% group_by(PopulationName) %>%
          #  filter(n_distinct(Year) > 1)

          surv_sh <- stringr::str_subset(survey_sh_names, "[S,s]urv")
          if(length(surv_sh)<1){
            stop("The spreadsheet does not include a sheet named 'surv'")
          }
          survey_surv <- googlesheets4::read_sheet(input$survey_url, surv_sh,
                                                   na = "NA") %>%
            bboudata::bbd_chk_data_survival(multi_pops = TRUE, allow_missing = TRUE)

          #survey_surv <- survey_surv %>% group_by(PopulationName) %>%
          #  filter(n_distinct(Year) > 1)

          pop_sh <- stringr::str_subset(survey_sh_names, "[P,p]opulation")
          if(length(pop_sh)<1){
            stop("The spreadsheet does not include a sheet named 'population'")
          }

          survey_pop <- googlesheets4::read_sheet(input$survey_url, pop_sh,
                                                  na = "NA")
          pop_nms <- purrr::map_lgl(list("PopulationName", "N", "Year"),
                                    \(x)stringr::str_detect(colnames(survey_pop), x) %>% any())

          if(!all(pop_nms)){
            stop("The population estimates sheet is missing the expected column names:",
                 paste0(colnames(survey_pop)[!pop_nms], collapse = ", "))
          }

          N0 <- survey_pop %>% group_by(PopulationName) %>% filter(Year == max(Year)) %>%
            rename(N0 = N)

          pops_run <- intersect(survey_recruit$PopulationName,
                                survey_surv$PopulationName) %>%
            intersect(N0$PopulationName)

          pop_file_in <- bbouMakeSummaryTable(
            survey_surv %>% filter(PopulationName %in% pops_run),
            survey_recruit %>% filter(PopulationName %in% pops_run),
            N0 = N0 %>% filter(PopulationName %in% pops_run),
            shiny_progress = TRUE, i18n = i18n, return_mcmc = TRUE)

          pop_fits <- pop_file_in
          pop_file_in <- pop_file_in$parTab

          #filter out populations with only one year of data at this stage
          rmPops <- unique(c(
            (survey_surv %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName,
            (survey_recruit %>% group_by(PopulationName) %>%  filter(n_distinct(Year) == 1))$PopulationName))

          pops_run <- setdiff(pops_run,rmPops)

          pop_file_in <- subset(pop_file_in, is.element(pop_name,pops_run))

          bbouMakeFigures(pop_fits$surv_fit, pop_fits$recruit_fit,
                          fig_dir = file.path(inst_dir, "www"),
                          i18n = i18n,
                          show_interpolated = FALSE)

          # Add description
          desc_sh <- stringr::str_subset(survey_sh_names, "[D,d]escription")
          if(length(desc_sh)<1){
            stop("The spreadsheet does not include a sheet named 'description'")
          }

          dat_desc <- googlesheets4::read_sheet(input$survey_url, desc_sh)

          desc_nms <- colnames(dat_desc)
          if(length(desc_nms) > 1){
            desc_nms <- stringr::str_subset(desc_nms,
                                           paste0("_",input$selected_language,"$"))
          }

          pop_file_in$description <- NA_character_
          pop_file_in$description[1] <- dat_desc[,desc_nms][[1]][1]

          end <- Sys.time()
          print(end - start)
        })

        # save the file locally so only re-run when asked
        write.csv(pop_file_in, file.path(inst_dir, "extdata", "temp_pop_file_local.csv"), row.names = FALSE)

        # update the reactive value
        pop_file(pop_file_in)

        shiny::removeModal()
        nav_select(id = "body", selected = "input_data_tab")
        do_update(FALSE)
      }
    )
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}

