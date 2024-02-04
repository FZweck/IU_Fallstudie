# Laden relevanter R packages
library(dplyr)
library(tidyr)
library(xgboost)
library(e1071)
library(caret)
library(MLmetrics)
library(pROC)
library(ROCR)

# Globale Objekte
load( "Robjects/models.Rdata")

best_model <- "model_xgboost_sparse"

vars <- c("amount_log", "amount", "country", "attempt", "secured", "card", 
          "tmsp_wday", "tmsp_is_weekend", "tmsp_hour", "tmsp_minute")
vars_scarse <- c("amount_log", "attempt", "secured", "card", "tmsp_hour", "tmsp_minute")

PSP <- c("Goldcard", "Moneycard", "Simplecard", "UK_Card")

## Auswertung #####
# Modelauswahl
model_type <- define_model_type(best_model)
my_model <- all_models[[best_model]]$model
if(model_type%in%c("logit","xgboost")) cutoff <- all_models[[best_model]]$cutoff
sparse <- grepl("sparse", best_model)

# Modellauswertung
recommend_PSP <- function(data, output_type="PSP"){
  # Bereite input vor
  input_data <- data %>% 
    mutate(id = 1:nrow(data))
  input_data <- input_data[rep(1:nrow(input_data), each = length(PSP)),]
  input_data$PSP <- PSP
  input_data <- input_data %>% 
    mutate(PSP = as.factor(PSP)) %>% 
    left_join(costs)
  
  # Triff Modelvorhersage
  pred <- get_predictions(my_model, input_data, model_type, sparse)
  if(model_type%in%c("logit","xgboost")) pred <- pred>cutoff
  
  input_data$success <- pred
  input_data$costs <- ifelse(input_data$success, input_data$cost_success, input_data$cost_failure)
  
  # Auswahl des PSP
  res <- input_data[input_data$success,]
  if(nrow(res)!=0){
    res <- res[order(res$costs),]
    res <- res[!duplicated(res$id),] 
  }
  
  res_failure <- input_data[!input_data$id%in%res$id,]
  if(nrow(res_failure)!=0){
    res_failure <- res_failure[order(res_failure$costs),]
    res_failure <- res_failure[!duplicated(res_failure$id),]
  }
  
  res <- rbind(res, res_failure)
  
  # Ausgabe
  res <- res[names(res)%in%c(names(data), "PSP", "success", "costs")]
  res <- res[order(res$id),]
  
  if(output_type=="PSP"){
    output <- res$PSP
  } else if(output_type=="result"){
    output <- res[,c("PSP", "success", "costs")]
  } else if(output_type=="full"){
    output <- res
  }
  return(output)
}
