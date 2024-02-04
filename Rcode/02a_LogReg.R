rm(list = ls())

# Laden relevanter R packages
library(caret)
library(MLmetrics)
library(pROC)
library(ROCR)

# Globale Objekte
load("Robjects/input_data.Rdata")

###################################
## Logistische Regression #########
###################################

## Gesamtdaten ####################
# Modelltraining
model_logit <- glm(success~PSP+amount_log+amount+country+attempt+secured+card+tmsp_wday+tmsp_is_weekend+tmsp_hour+tmsp_minute, 
                   family="binomial", 
                   data=train_data
)
summary(model_logit)
anova(model_logit)

# Speichere Modell
model_file <- list(
  model = model_logit,
  model_family = "Logistische Regression",
  model_type = "Originaldaten, alle Parameter"
)
save(model_file, file = "Robjects/models/model_logit.Rdata")

## Undersampling ##################
# Model training
model_logit <- glm(success~PSP+amount_log+amount+country+attempt+secured+card+tmsp_wday+tmsp_is_weekend+tmsp_hour+tmsp_minute, 
                   family="binomial", 
                   data=train_data_us
)
summary(model_logit)
anova(model_logit)

# Speichere Modell
model_file <- list(
  model = model_logit,
  model_family = "Logistische Regression",
  model_type = "Under Sampling, alle Parameter"
)
save(model_file, file = "Robjects/models/model_logit_us.Rdata")

## Variablenauswahl Gesamtdaten #####
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

## Variablenauswahl Undersampling ####
# Model training
model_logit <- glm(success~PSP+amount_log+attempt+secured+card+tmsp_hour+tmsp_minute, 
                   family="binomial", 
                   data=train_data_us
)
summary(model_logit)
anova(model_logit)

# Speichere Modell
model_file <- list(
  model = model_logit,
  model_family = "Logistische Regression",
  model_type = "Under Sampling, Parameterauswahl"
)
save(model_file, file = "Robjects/models/model_logit_us_sparse.Rdata")




