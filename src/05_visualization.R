source("src/00_packages.R")

plot_country_comparisons <- function(rakete) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow = c(2, 2))
  
  barplot(
    table(rakete$klasa_po_dometu[rakete$drzava == 2]),
    main = "Kina",
    col = "lightblue",
    xlab = "Klasa po dometu",
    ylab = "Frequency"
  )
  
  barplot(
    table(rakete$klasa_po_dometu[rakete$drzava == 1]),
    main = "Rusija",
    col = "lightgreen",
    xlab = "Klasa po dometu",
    ylab = "Frequency"
  )
  
  barplot(
    table(rakete$nacin_lansiranja[rakete$drzava == 2]),
    main = "Kina",
    col = "lightblue",
    xlab = "Nacin lansiranja",
    ylab = "Frequency"
  )
  
  barplot(
    table(rakete$nacin_lansiranja[rakete$drzava == 1]),
    main = "Rusija",
    col = "lightgreen",
    xlab = "Nacin lansiranja",
    ylab = "Frequency"
  )
}