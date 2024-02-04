rm(list = ls())

# Laden relevanter R packages
library(e1071)
library(caret)
library(MLmetrics)
library(pROC)
library(ROCR)

# Globale Objekte
load("Robjects/input_data.Rdata")

###################################
## Support Vector Machines ########
###################################

## Gesamtmodell ###################
# Initales Model
model_init <- svm(success~PSP+amount_log+amount+country+attempt+secured+card+tmsp_wday+tmsp_is_weekend+tmsp_hour+tmsp_minute, 
                  kernel = "radial",
                  type = "C-classification",
                  probability = T,
                  data = train_data_us
)

# Hyperparameter tuning
svm_tune <-  tune(svm,
                  success~PSP+amount_log+amount+country+attempt+secured+card+tmsp_wday+tmsp_is_weekend+tmsp_hour+tmsp_minute, 
                  data = hpo_data_us,
                  kernel = "radial",
                  type = "C-classification",
                  ranges = list(gamma = c(0.001, 0.01, 0.1, 1, 10, 100), cost = c(0.001, 0.01, 0.1, 1, 10, 100)),
                  tunecontrol = tune.control(sampling = "cross", cross = 10)
)
params <- svm_tune$best.parameters

# Finales model
model_svm <- svm(success~PSP+amount_log+amount+country+attempt+secured+card+tmsp_wday+tmsp_is_weekend+tmsp_hour+tmsp_minute, 
                 data = train_data_us,
                 kernel = "radial",
                 type = "C-classification",
                 probability = T,
                 gamma = 0.1,
                 cost = 1
)

# Speichere Modell
model_file <- list(
  model = model_init,
  model_family = "SVM (Inital)",
  model_type = "Undersamlping, alle Parameter"
)
save(model_file, file = "Robjects/models/model_svm_init.Rdata")

model_file <- list(
  model = model_svm,
  model_family = "SVM (Optimiert)",
  model_type = "Undersamlping, alle Parameter"
)
save(model_file, file = "Robjects/models/model_svm.Rdata")

## Variablenauswahl ###############
# Initales Model
model_init <- svm(success~PSP+amount_log+attempt+secured+card+tmsp_hour+tmsp_minute, 
                  kernel = "radial",
                  type = "C-classification",
                  probability = T,
                  data = train_data_us
)

# Hyperparameter tuning
svm_tune <-  tune(svm,
                  success~PSP+amount_log+attempt+secured+card+tmsp_hour+tmsp_minute, 
                  data = hpo_data_us,
                  kernel = "radial",
                  type = "C-classification",
                  ranges = list(gamma = c(0.001, 0.01, 0.1, 1, 10, 100), cost = c(0.001, 0.01, 0.1, 1, 10, 100)),
                  tunecontrol = tune.control(sampling = "cross", cross = 10)
)
params <- svm_tune$best.parameters

# Finales model
model_svm <- svm(success~PSP+amount_log+attempt+secured+card+tmsp_hour+tmsp_minute, 
                 data = train_data_us,
                 kernel = "radial",
                 type = "C-classification",
                 probability = T,
                 gamma = 0.1,
                 cost = 1
)

# Speichere Modell
model_file <- list(
  model = model_init,
  model_family = "SVM (Inital)",
  model_type = "Undersamlping, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_svm_sparse_init.Rdata")

model_file <- list(
  model = model_svm,
  model_family = "SVM (Optimiert)",
  model_type = "Undersamlping, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_svm_sparse.Rdata")
