addl_params_ui <- function(id, mod_defaults, i18n, name){
  accordion(
    open = FALSE,
    id = paste0("addl_params_", id),
    accordion_panel(
    title = paste0(name, ": ", i18n$t("Additional model parameters for scenario")),
    sliderInput(paste0("P_0", "_", id),
                label = i18n$t("Maximum recruitment multiplier"),
                value = mod_defaults$P_0, min = 0, max = 1
    ),
    sliderInput(paste0("P_K", "_", id),
                label = i18n$t("Recruitment multiplier at carrying capacity"),
                value = mod_defaults$P_K, min = 0, max = 1
    ),
    numericInput(paste0("a", "_", id),
                 label = i18n$t("Density dependence shape parameter"),
                 value = mod_defaults$a
    ),
    numericInput(paste0("b", "_", id),
                 label = i18n$t("Allee effect parameter"),
                 value = mod_defaults$b
    ),
    numericInput(paste0("K", "_", id),
                 label = i18n$t("Carrying capacity"),
                 value = mod_defaults$K
    ),
    numericInput(paste0("r_max", "_", id),
                 label = i18n$t("Maximum population growth rate"),
                 value = mod_defaults$r_max
    ),
    sliderInput(paste0("s", "_", id),
                label = i18n$t("Sex ratio"),
                value = mod_defaults$s, min = 0, max = 1
    )
  ))
}

get_addl_params <- function(id, input){
  addl_params <- c('P_0', 'P_K', 'a', 'b', 'K', 'r_max', 's')
  addl_params <- paste0(addl_params, "_", id)

  addl_params <- addl_params %>%
    map(\(x) input[[x]]) %>% set_names(addl_params)
  # remove suffixs from argument names
  set_names(addl_params, names(addl_params) %>% str_remove(paste0("_", id)))

}
