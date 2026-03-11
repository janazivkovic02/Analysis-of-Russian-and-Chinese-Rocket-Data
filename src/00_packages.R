required_packages <- c(
  "readODS",
  "dplyr",
  "zoo",
  "corrplot",
  "caret",
  "car",
  "glmnet",
  "pROC",
  "MLmetrics",
  "randomForest",
  "smotefamily",
  "dbscan",
  "ggplot2",
  "tidyr"
)

invisible(lapply(required_packages, library, character.only = TRUE))