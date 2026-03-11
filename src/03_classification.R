source("src/00_packages.R")

prepare_classification_data <- function(rakete) {
  rakete_za_klasifikaciju <- rakete %>%
    mutate(
      kopneno_lansiranje = case_when(
        grepl("^SS-\\d+$", NATO_ime) ~ 0,
        grepl("^SS-X-\\d+$", NATO_ime) ~ 0,
        grepl("^SS-N-\\d+$", NATO_ime) ~ 1,
        grepl("^AS-\\d+$", NATO_ime) ~ 2,
        grepl("^CSS-\\d+$", NATO_ime) ~ 0,
        grepl("^CSS-N-\\d+$", NATO_ime) ~ 1,
        grepl("^CSS-X-\\d+$", NATO_ime) ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      kopneno_lansiranje = case_when(
        kopneno_lansiranje == 0 ~ 1,
        kopneno_lansiranje == 1 ~ 0,
        kopneno_lansiranje == 2 ~ 0
      )
    ) %>%
    select(-NATO_ime, -rusko_ime) %>%
    mutate(
      fiksni_lanser = case_when(
        nacin_lansiranja == 1 ~ 1,
        nacin_lansiranja == 2 ~ 0,
        nacin_lansiranja == 3 ~ 0,
        nacin_lansiranja == 4 ~ 0
      )
    ) %>%
    select(-nacin_lansiranja)
  
  rakete_za_klasifikaciju$fiksni_lanser <-
    as.factor(rakete_za_klasifikaciju$fiksni_lanser)
  
  return(rakete_za_klasifikaciju)
}

evaluate_random_forest <- function(rakete_za_klasifikaciju) {
  set.seed(126)
  foldovi <- createFolds(rakete_za_klasifikaciju$fiksni_lanser, k = 5)
  
  cv_random_forest <- lapply(foldovi, function(fold_indeksi) {
    train_data <- rakete_za_klasifikaciju[-fold_indeksi, ]
    test_data <- rakete_za_klasifikaciju[fold_indeksi, ]
    
    train_data$fiksni_lanser <- as.factor(train_data$fiksni_lanser)
    test_data$fiksni_lanser <- as.factor(test_data$fiksni_lanser)
    
    w1 <- sum(train_data$fiksni_lanser == 1) / length(train_data$fiksni_lanser)
    w0 <- 1
    
    rf_model <- randomForest(
      fiksni_lanser ~ .,
      data = train_data,
      mtry = 3,
      ntree = 500,
      classwt = c(w0, w1)
    )
    
    test_predikcije <- predict(rf_model, test_data, type = "prob")[, 2]
    
    auc <- roc(test_data$fiksni_lanser, test_predikcije)$auc
    binarne_predikcije <- ifelse(test_predikcije > 0.5, 1, 0)
    f1 <- F1_Score(test_data$fiksni_lanser, binarne_predikcije)
    tacnost <- mean(binarne_predikcije == test_data$fiksni_lanser)
    
    c(auc = auc, tacnost = tacnost, f1 = f1)
  })
  
  met <- do.call(rbind, cv_random_forest)
  
  data.frame(
    Metrika = c("Tačnost", "AUC", "F1-score"),
    Vrednost = c(mean(met[, "tacnost"]), mean(met[, "auc"]), mean(met[, "f1"]))
  )
}

prepare_smote_data <- function(rakete_za_klasifikaciju) {
  balansirani_podaci <- SMOTE(
    rakete_za_klasifikaciju[, -12],
    rakete_za_klasifikaciju[, 12],
    K = 3
  )
  
  balansirani_podaci <- balansirani_podaci$data
  balansirani_podaci$class <- as.numeric(balansirani_podaci$class)
  
  return(balansirani_podaci)
}

evaluate_ridge_classifier <- function(balansirani_podaci) {
  set.seed(126)
  foldovi <- createFolds(balansirani_podaci$class, k = 13)
  
  cv_results_ridge <- lapply(foldovi, function(fold_indeksi) {
    train_data <- balansirani_podaci[-fold_indeksi, ]
    test_data <- balansirani_podaci[fold_indeksi, ]
    
    X_train <- as.matrix(train_data[, -12])
    y_train <- train_data$class
    X_test <- as.matrix(test_data[, -12])
    y_test <- test_data$class
    
    ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)
    najbolje_lambda <- ridge_model$lambda.min
    final_ridge_model <- glmnet(X_train, y_train, alpha = 1, lambda = najbolje_lambda)
    
    predikcije <- predict(final_ridge_model, X_test, type = "response")
    binarne_predikcije <- ifelse(predikcije > 0.5, 1, 0)
    
    tacnost <- mean(binarne_predikcije == y_test)
    f1 <- F1_Score(y_test, binarne_predikcije, positive = "1")
    roc_curve <- roc(response = y_test, predictor = as.vector(predikcije))
    auc <- auc(roc_curve)
    
    c(auc = auc, tacnost = tacnost, f1 = f1)
  })
  
  met <- do.call(rbind, cv_results_ridge)
  
  data.frame(
    Metrika = c("Tačnost", "AUC", "F1-score"),
    Vrednost = c(mean(met[, "tacnost"]), mean(met[, "auc"]), mean(met[, "f1"]))
  )
}

fit_final_ridge_classifier <- function(balansirani_podaci) {
  X <- as.matrix(balansirani_podaci[, -12])
  y <- balansirani_podaci$class
  
  ridge_model <- cv.glmnet(X, y, alpha = 0)
  najbolje_lambda <- ridge_model$lambda.min
  finalni_ridge_model <- glmnet(X, y, alpha = 1, lambda = najbolje_lambda)
  
  finalni_ridge_model
}