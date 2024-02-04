rm(list = ls())

# Laden relevanter R packages
library(dplyr)
library(tidyr)
library(xgboost)
library(caret)
library(MLmetrics)
library(pROC)
library(ROCR)

# Globale Objekte
load("Robjects/input_data.Rdata")
path <- "Robjects/xgboost_hpo_sparse_all"

set.seed(76438)

############
# Daten für XGBoost vorbereiten
train_x <- train_data_us %>% 
  select(PSP, amount_log, attempt, secured, card, tmsp_hour, tmsp_minute)
train_x <- model.matrix(~., train_x)
train_y <- train_data_us$success

test_x <- test_data %>% 
  select(PSP, amount_log, attempt, secured, card, tmsp_hour, tmsp_minute)
test_x <- model.matrix(~., test_x)
test_y <- test_data$success

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

watchlist <- list(train=xgb_train, test=xgb_test)

# Initiales Model
model_init <- xgb.train(data = xgb_train, 
                        watchlist = watchlist, 
                        objective = "binary:logistic",
                        max.depth = 3, 
                        nrounds = 200)

# Auswertung der Hyperparameter-Optimierung
full_model_list <- list()

all_files <- list.files(path)
for(filename in all_files){
  load(paste(path, filename, sep = "/"))
  full_model_list <- append(full_model_list, part_model_list)
}

hpo <- data.frame(t(sapply(full_model_list, `[[`, "params")))
hpo <- cbind(hpo, t(sapply(full_model_list, function(x) x$evaluation_log[nrow(x$evaluation_log)])))
hpo <- apply(hpo, 2, unlist)
hpo <- apply(hpo[,!colnames(hpo)%in%c("objective", "eval_metric", "eval_metric.1", "eval_metric.2")], 2, as.numeric)
hpo <- data.frame(hpo)

final_params <- hpo[hpo$train_aucpr_mean==max(hpo$train_aucpr_mean),]

# Training des finalen Models
model_xgb <- xgb.train(data = xgb_train, 
                       watchlist = watchlist, 
                       objective = "binary:logistic",
                       eta = final_params$eta,
                       max.depth = final_params$max_depth,
                       min_child_weight = final_params$min_child_weight,
                       gamma = final_params$gamma,
                       colsample_bytree = final_params$colsample_bytree,
                       early_stopping_rounds = 10,
                       nrounds = 1000,
                       verbose = 0,
                       eval_metric = "aucpr", eval_metric = "auc", eval_metric = "logloss"
)

# Feature importance 
importance_matrix = xgb.importance(colnames(xgb_train), model = model_xgb)
xgb.plot.importance(importance_matrix)

# Speichere Modelle
model_file <- list(
  model = model_init,
  model_family = "XGBoost (Inital)",
  model_type = "Under Sampling, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_xgboost_sparse_us_init.Rdata")

model_file <- list(
  model = model_xgb,
  model_family = "XGBoost (Optimiert)",
  model_type = "Undersampling, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_xgboost_sparse_us.Rdata")
