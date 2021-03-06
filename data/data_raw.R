library(dplyr)
library(DBI)
library(RSQLite)
# plants <- read.csv2("~/Downloads/species.csv", sep = "\t")

clean_logical <- function(x) {
  if ("false" %in% x || "true" %in% x) {
    x <- replace(x, x == "false", FALSE)
    x <- replace(x, x == "true", TRUE)
    return(as.logical(x))
  }
  x
}
clean_character <- function(x) {
  replace(x, x == "" | x == "NULL" | x == "NA" | x == "<NA>", NA)
}

plants_clean <- plants %>% 
  mutate_if(is.character, clean_character) %>% 
  mutate_if(is.character, clean_logical) %>% 
  select_if(~ sum(!is.na(.)) > 0)
summary(plants_clean)

lapply(plants_clean, function(x) {
  any(c("NULL", "null", "false", "true", "NA", "na") %in% x)
})


con <- dbConnect(SQLite(), "data/plants.sqlite")
dbWriteTable(con, "plants", plants_clean, overwrite = TRUE)