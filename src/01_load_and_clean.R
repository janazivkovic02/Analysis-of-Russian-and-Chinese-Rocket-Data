source("src/00_packages.R")

load_and_clean_data <- function(path = "data/rakete.ods") {
  rakete <- read_ods(path, sheet = 1)
  
  rakete <- rakete %>%
    mutate(
      klasa_po_dometu = case_when(
        klasa_po_dometu == "S" ~ 1,
        klasa_po_dometu == "M" ~ 2,
        klasa_po_dometu == "L" ~ 3
      ),
      tip_goriva = case_when(
        tip_goriva == "Tecno" ~ 1,
        tip_goriva == "Cvrsto" ~ 2
      ),
      nacin_lansiranja = case_when(
        nacin_lansiranja == "Silos" ~ 1,
        nacin_lansiranja == "RM TEL" ~ 2,
        nacin_lansiranja == "Mornaricki" ~ 3,
        nacin_lansiranja == "Avion" ~ 4
      )
    )
  
  rakete$NATO_ime[39] <- "CSS-X-11"
  rakete$NATO_ime[33] <- "CSS-18"
  rakete$NATO_ime[16] <- "AS-24"
  
  rakete <- rakete %>%
    mutate(
      drzava = case_when(
        startsWith(NATO_ime, "SS") ~ "R",
        startsWith(NATO_ime, "CSS") ~ "Ch",
        TRUE ~ "Unknown"
      )
    )
  
  rakete$drzava[16] <- "R"
  
  rakete <- rakete %>%
    mutate(
      drzava = case_when(
        drzava == "R" ~ 1,
        drzava == "Ch" ~ 2
      )
    )
  
  rakete <- rakete %>%
    mutate(NATO_ime = gsub("(CSS-\\d+).*", "\\1", NATO_ime)) %>%
    mutate(NATO_ime = gsub("(CSS-N-\\d+).*", "\\1", NATO_ime)) %>%
    mutate(NATO_ime = gsub("(SS-\\d+).*", "\\1", NATO_ime)) %>%
    mutate(NATO_ime = gsub("(SS-N-\\d+).*", "\\1", NATO_ime)) %>%
    mutate(NATO_ime = gsub("(SS-X-\\d+).*", "\\1", NATO_ime))
  
  return(rakete)
}

get_rows_with_na <- function(rakete) {
  rakete[apply(rakete, 1, function(row) any(is.na(row))), ]
}

get_dong_feng_rows <- function(rakete) {
  rakete[grepl("^Dong Feng \\d+", rakete$rusko_ime), ]
}