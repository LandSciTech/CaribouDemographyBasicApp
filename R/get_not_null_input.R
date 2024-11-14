get_not_null_input <- function(x, inp){
  nms <- str_subset(names(inp), x)
  out_lst <- map(nms, \(x)inp[[x]]) %>% set_names(nms) %>%
    compact()
}
