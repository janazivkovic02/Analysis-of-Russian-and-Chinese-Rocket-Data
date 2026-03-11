source("src/00_packages.R")

impute_missing_values <- function(rakete) {
  rakete_dong_feng <- rakete[grepl("^Dong Feng \\d+", rakete$rusko_ime), ]
  rakete_dong_feng_reduced <- rakete_dong_feng[-c(8, 9), ]
  
  interpolacija <- rakete_dong_feng_reduced
  posmatrane_kolone <- c("masa_kg", "duzina_m")
  
  for (col in posmatrane_kolone) {
    interpolacija[[col]] <- na.approx(
      rakete_dong_feng_reduced[[col]],
      na.rm = FALSE
    )
  }
  
  rakete[31, 3] <- interpolacija[4, 3]
  rakete[31, 4] <- interpolacija[4, 4]
  
  preProc <- preProcess(
    rakete[-33, c(3, 4, 5, 7, 8)],
    method = "pca",
    pcaComp = 2
  )
  
  PCA <- predict(preProc, rakete[-33, c(3, 4, 5, 7, 8)])
  PCA$broj_podglava <- rakete$broj_podglava[-33]
  
  model <- train(broj_podglava ~ ., data = PCA, method = "lm")
  
  testPCA <- predict(preProc, rakete[33, c(3, 4, 5, 7, 8)])
  broj_podglava_NA <- predict(model, newdata = testPCA)
  broj_podglava_NA <- round(broj_podglava_NA)
  
  rakete[33, 6] <- broj_podglava_NA
  
  return(rakete)
}

plot_correlation_matrix <- function(rakete) {
  corrplot(cor(rakete[-33, -c(1, 2)]))
}

fit_range_models <- function(rakete) {
  linearni_model <- lm(max_domet_km ~ . - rusko_ime - NATO_ime, data = rakete)
  
  linearni_model_2 <- lm(
    max_domet_km ~ duzina_m + payload_kg + broj_faza_motora +
      klasa_po_dometu + tip_goriva,
    data = rakete
  )
  
  anova_rez <- anova(linearni_model_2, linearni_model)
  vif_rez <- vif(linearni_model_2)
  
  set.seed(126)
  foldovi <- createFolds(rakete$max_domet_km, k = 13)
  df <- rakete[, -c(1, 2)]
  
  cv_results_lm <- lapply(foldovi, function(fold_indeks) {
    train_data <- df[-fold_indeks, ]
    test_data <- df[fold_indeks, ]
    
    model <- lm(max_domet_km ~ ., data = train_data)
    predikcije <- predict(model, test_data)
    
    stvarne_vrednosti <- test_data$max_domet_km
    1 - sum((stvarne_vrednosti - predikcije)^2) /
      sum((stvarne_vrednosti - mean(stvarne_vrednosti))^2)
  })
  r2_linearni_model_2 <- mean(unlist(cv_results_lm))
  
  cv_results_lasso <- lapply(foldovi, function(fold_indeksi) {
    train_data <- rakete[-fold_indeksi, ]
    test_data <- rakete[fold_indeksi, ]
    
    X_train <- as.matrix(train_data[, -c(1, 2, 9)])
    y_train <- train_data$max_domet_km
    X_test <- as.matrix(test_data[, -c(1, 2, 9)])
    y_test <- test_data$max_domet_km
    
    lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
    najbolje_lambda <- lasso_model$lambda.min
    predikcije <- predict(lasso_model, newx = X_test, s = najbolje_lambda)
    
    1 - sum((y_test - predikcije)^2) / sum((y_test - mean(y_test))^2)
  })
  r2_lasso <- mean(unlist(cv_results_lasso))
  
  cv_results_ridge <- lapply(foldovi, function(fold_indeksi) {
    train_data <- rakete[-fold_indeksi, ]
    test_data <- rakete[fold_indeksi, ]
    
    X_train <- as.matrix(train_data[, -c(1, 2, 9)])
    y_train <- train_data$max_domet_km
    X_test <- as.matrix(test_data[, -c(1, 2, 9)])
    y_test <- test_data$max_domet_km
    
    ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)
    najbolje_lambda <- ridge_model$lambda.min
    predikcije <- predict(ridge_model, newx = X_test, s = najbolje_lambda)
    
    1 - sum((y_test - predikcije)^2) / sum((y_test - mean(y_test))^2)
  })
  r2_ridge <- mean(unlist(cv_results_ridge))
  
  cv_results_PCA <- lapply(foldovi, function(fold_indeksi) {
    train_data <- rakete[-fold_indeksi, ]
    test_data <- rakete[fold_indeksi, ]
    
    preProc <- preProcess(train_data[, -c(1, 2, 9)], method = "pca", pcaComp = 5)
    trainPCA <- predict(preProc, train_data[, -c(1, 2, 9)])
    testPCA <- predict(preProc, test_data[, -c(1, 2, 9)])
    
    trainPCA$max_domet_km <- train_data$max_domet_km
    testPCA$max_domet_km <- test_data$max_domet_km
    
    model <- train(max_domet_km ~ ., data = trainPCA, method = "lm")
    predikcije <- predict(model, newdata = testPCA)
    
    1 - sum((test_data$max_domet_km - predikcije)^2) /
      sum((test_data$max_domet_km - mean(test_data$max_domet_km))^2)
  })
  r2_PCA <- mean(unlist(cv_results_PCA))
  
  rezultati <- data.frame(
    Model = c("Linearni model 2", "Lasso", "Ridge", "PCA"),
    Mean_R2 = c(r2_linearni_model_2, r2_lasso, r2_ridge, r2_PCA)
  )
  
  final_preProc <- preProcess(rakete[, -c(1, 2, 9)], method = "pca", pcaComp = 5)
  raketePCA <- predict(final_preProc, rakete[, -c(1, 2, 9)])
  raketePCA$max_domet_km <- rakete$max_domet_km
  finalni_model <- train(max_domet_km ~ ., data = raketePCA, method = "lm")
  
  list(
    linearni_model = linearni_model,
    linearni_model_2 = linearni_model_2,
    anova = anova_rez,
    vif = vif_rez,
    cv_results = rezultati,
    final_preProc = final_preProc,
    finalni_model = finalni_model
  )
}