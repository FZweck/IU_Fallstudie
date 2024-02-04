rm(list = ls())

max_res <- 500
step <- 100
batches <- seq(0,max_res, by=step)

path <- "Robjects/xgboost_hpo_sparse_us"
batch <- length(list.files(path))+1
filename <- paste0(path, "/res", batch, ".Rdata")
save(batch, file = filename)

# Laden relevanter R packages
library(dplyr)
library(tidyr)
library(xgboost)
library(caret)
library(MLmetrics)
library(pROC)

# Globale Objekte
load("Robjects/input_data.Rdata")

set.seed(76438)

############
# Daten für XGBoost vorbereiten
train_x <- hpo_data %>% 
  select(PSP, amount_log, attempt, secured, card, tmsp_hour, tmsp_minute)
train_x <- model.matrix(~., train_x)
train_y <- hpo_data$success

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)

# Hyperparameter-Optimierung
gs <- crossing(eta = seq(0.05,1, 0.05),
               max.depth = 2:7,
               min_child_weight = c(1,3,5,7),
               gamma = seq(0.1,0.6, 0.1),
               colsample_bytree = seq(0.2,1, 0.2)
)
sample <- sample(1:nrow(gs), 500)
gs <- gs[sample,]

part_model_list <- list()

start <- Sys.time()
for(i in (batches[batch]+1):batches[batch+1]){
  print(i)
  model_hpo <- xgb.cv(data = xgb_train, 
                      nfold = 10,
                      objective = "binary:logistic",
                      eta = gs$eta[i],
                      max.depth = gs$max.depth[i],
                      min_child_weight = gs$min_child_weight[i],
                      gamma = gs$gamma[i],
                      colsample_bytree = gs$colsample_bytree[i],
                      early_stopping_rounds = 10,
                      nrounds = 1000,
                      verbose = 0,
                      metrics = list("aucpr", "auc", "logloss"))
  
  part_model_list <- append(part_model_list, list(model_hpo))
}
end <- Sys.time()
start - end

save(part_model_list, file = filename)

