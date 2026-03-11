source("src/00_packages.R")

prepare_clustering_data <- function(rakete) {
  rakete_za_klasterizaciju <- rakete[rakete$klasa_po_dometu != 2, ] %>%
    select(-max_domet_km, -klasa_po_dometu, -NATO_ime, -rusko_ime)
  
  return(rakete_za_klasterizaciju)
}

run_dbscan_clustering <- function(rakete_za_klasterizaciju, eps = 15000, minPts = 9) {
  dbscan::dbscan(rakete_za_klasterizaciju, eps = eps, minPts = minPts)
}

plot_knn_distance <- function(rakete_za_klasterizaciju) {
  kNNdistplot(rakete_za_klasterizaciju, k = 8)
  abline(h = 15000)
}

plot_dbscan_clusters <- function(rakete_za_klasterizaciju, dbscan_model) {
  plot(rakete_za_klasterizaciju, col = dbscan_model$cluster)
}

summarize_clusters <- function(rakete_za_klasterizaciju, dbscan_model) {
  prvi_klaster <- rakete_za_klasterizaciju[dbscan_model$cluster == 1, ]
  drugi_klaster <- rakete_za_klasterizaciju[dbscan_model$cluster == 2, ]
  noise_points <- rakete_za_klasterizaciju[dbscan_model$cluster == 0, ]
  
  list(
    prvi_klaster = summary(prvi_klaster),
    drugi_klaster = summary(drugi_klaster),
    noise_points = summary(noise_points)
  )
}

assign_final_clusters <- function(rakete_za_klasterizaciju, dbscan_model) {
  rakete_za_klasterizaciju$klasterizacija <- dbscan_model$cluster
  rakete_za_klasterizaciju$klasterizacija[dbscan_model$cluster == 0] <- 1
  rakete_za_klasterizaciju
}