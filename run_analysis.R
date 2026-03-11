source("src/01_load_and_clean.R")
source("src/02_imputation_and_regression.R")
source("src/03_classification.R")
source("src/04_clustering.R")
source("src/05_visualization.R")

rakete <- load_and_clean_data("data/rakete.ods")

cat("\n--- Redovi sa nedostajucim vrednostima ---\n")
print(get_rows_with_na(rakete))

cat("\n--- Dong Feng rakete ---\n")
print(get_dong_feng_rows(rakete))

rakete <- impute_missing_values(rakete)

cat("\n--- Korelaciona matrica ---\n")
plot_correlation_matrix(rakete)

cat("\n--- Modeli za predvidjanje dometa ---\n")
range_models <- fit_range_models(rakete)
print(summary(range_models$linearni_model))
print(summary(range_models$linearni_model_2))
print(range_models$anova)
print(range_models$vif)
print(range_models$cv_results)

cat("\n--- Klasifikacija ---\n")
rakete_za_klasifikaciju <- prepare_classification_data(rakete)
rf_results <- evaluate_random_forest(rakete_za_klasifikaciju)
print(rf_results)

balansirani_podaci <- prepare_smote_data(rakete_za_klasifikaciju)
ridge_results <- evaluate_ridge_classifier(balansirani_podaci)
print(ridge_results)

finalni_ridge_model <- fit_final_ridge_classifier(balansirani_podaci)
print(finalni_ridge_model)

cat("\n--- Klasterizacija ---\n")
rakete_za_klasterizaciju <- prepare_clustering_data(rakete)
plot_knn_distance(rakete_za_klasterizaciju)

dbscan_model <- run_dbscan_clustering(rakete_za_klasterizaciju)
plot_dbscan_clusters(rakete_za_klasterizaciju, dbscan_model)
print(dbscan_model$cluster)

cluster_summary <- summarize_clusters(rakete_za_klasterizaciju, dbscan_model)
print(cluster_summary$prvi_klaster)
print(cluster_summary$drugi_klaster)
print(cluster_summary$noise_points)

final_clusters <- assign_final_clusters(rakete_za_klasterizaciju, dbscan_model)
print(final_clusters$klasterizacija)

cat("\n--- Vizuelizacije po drzavi ---\n")
plot_country_comparisons(rakete)