# Create csv of translated values based on R code that is tagged with i18n and
# translate them using google translate

library(gtranslate)
library(tidyverse)
library(shiny.i18n)

# Create csv with english phrases to translate
create_translation_file(here::here("R/app.R"),
                        type = "csv", output = "inst/extdata/translations/translation_1.csv")

list.files("R", full.names = TRUE) %>%
  iwalk(\(x, i){
    create_translation_file(
      x, type = "csv",
      output = paste0("inst/extdata/translations/translation_", i+1,".csv")
    )
  })

# Combine all the files and use gtranslate to translate them
to_trans <- list.files(here::here("inst/extdata/translations"), pattern = "\\d.csv", full.names = TRUE) %>%
  map_dfr(read_csv) %>%
  distinct() %>%
  mutate(en = key, fr = translate(en, to = "fr", trim_str = FALSE), .keep = "none")

list.files(here::here("inst/extdata/translations"), pattern = "\\d.csv", full.names = TRUE) %>%
  map(file.remove)


# Save the csv that the app will use to translate
write_csv(to_trans, "inst/extdata/translations/translation_fr.csv")
