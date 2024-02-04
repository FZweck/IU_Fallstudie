rm(list = ls())

# Laden relevanter R packages
library(caret)
library(MLmetrics)
library(pROC)
library(ROCR)

# Globale Objekte
load("Robjects/input_data.Rdata")

############
# Model training
model_logit <- glm(success~PSP+amount_log+attempt+secured+card+tmsp_hour+tmsp_minute, 
                   family="binomial", 
                   data=train_data
)
summary(model_logit)
anova(model_logit)

# Speichere Modell
model_file <- list(
  model = model_logit,
  model_family = "Logistische Regression",
  model_type = "Originaldaten, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_logit_sparse.Rdata")

