rm(list = ls())

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
load("Robjects/input_data.Rdata")
path <- "Robjects/models"

## Hilfsfunktionen ######
define_model_type <- function(model_name){
  if(grepl("logit", model_name)){
    model_type <- "logit"
  } else if(grepl("xgboost", model_name)){
    model_type <- "xgboost"
  } else if(grepl("svm", model_name)){
    model_type <- "svm"
  }
  return(model_type)
}

get_predictions <- function(model, data, type, sparse){
  if(type=="logit"){
    pred <- predict(model, newdata = data, type = "response")
  } else if(type=="xgboost"){
    if(sparse){
      test_x <- data %>% 
        select(PSP, amount_log, attempt, secured, card, tmsp_hour, tmsp_minute)
      test_x <- model.matrix(~., test_x)
      test_y <- test_data$success
    } else {
      test_x <- data %>% 
        select(PSP, amount_log, amount, country, attempt, secured, card, tmsp_wday, 
               tmsp_is_weekend, tmsp_hour, tmsp_minute)
      test_x <- model.matrix(~., test_x)
      test_y <- test_data$success
    }
    
    pred <- predict(model,
                    test_x,
                    type = "response")
  } else if(type=="svm"){
    pred <- predict(model, 
                        newdata = test_data)
    pred <- pred=="TRUE"
  }
  
  return(pred)
}

gain_lift_charts <- function(pred, act, main = NULL, sub = NULL){
  if(is.null(main)) main = ""
  
  preds <- prediction(pred, act)
  gain <- performance(preds,"tpr","fpr")
  plot(gain, lwd=2, main = paste("Gainchart", main), sub = sub)
  
  lift <- performance(preds,"lift","rpp")
  plot(lift, lwd=2, main = paste("Liftchart", main), sub = sub)
}

eval_model <- function(pred, act, main, sub){
  if(is.null(main)) main = ""
  
  conf_matrix <- caret::confusionMatrix(as.factor(pred), as.factor(act), positive = "TRUE")
  
  precision <- Precision(act,pred, positive = "TRUE")
  recall <- Sensitivity(act,pred, positive = "TRUE")
  F1 <- F1_Score(act,pred, positive = "TRUE")
  accuracy <- Accuracy(act,pred)
  auc <- AUC(act,pred)
  roc <- roc(as.numeric(act), as.numeric(pred))
  
  print(conf_matrix)
  cat(paste("Precision:", round(precision,8), "\nRecall", round(recall,8), 
            "\nF1:", round(F1,8), "\nAcuraccy:", round(accuracy,8), 
            "\nAUC:", round(auc,8), "\n"))
  
  plot(roc, main = paste("ROC-Kurve", main), sub = sub)
  
  object <- list(
    precision = precision,
    recall = recall,
    F1 = F1,
    accuracy = accuracy,
    auc = auc,
    roc = roc
  )
  return(object)
}

################
all_files <- list.files(path)
all_models <- list()

## Lade Modelle
for(filename in all_files){
  # Modell-Name und -Typ
  model_name <-  gsub(".Rdata", "", filename, fixed = T)
  model_type <- define_model_type(model_name)
  sparse <- grepl("sparse", model_name)
  
  # Lade Modell
  load(paste(path, filename, sep = "/"))
  
  # Modellvorhersage
  pred <- get_predictions(model_file$model, test_data, model_type, sparse)
  
  # Gain & Lift charts
  if(model_type%in%c("logit", "xgboost")){
    gain_lift_charts(pred, as.numeric(test_data$success), paste("-", model_file$model_family), model_file$model_type)
  }
  
  # Ausgabe der Ergebnisse
  model_file$pred <- pred
  object <- list(model_file)
  names(object) <- model_name
  all_models <- append(all_models, object)
}

## Setze Schwellenwerte
all_models$model_logit$cutoff <- 0.2
all_models$model_logit_sparse$cutoff <- 0.2
all_models$model_logit_us$cutoff <- 0.46
all_models$model_logit_us_sparse$cutoff <- 0.46
all_models$model_xgboost$cutoff <- 0.2
all_models$model_xgboost_init$cutoff <- 0.2
all_models$model_xgboost_sparse$cutoff <- 0.2
all_models$model_xgboost_sparse_init$cutoff <- 0.2
all_models$model_xgboost_sparse_us$cutoff <- 0.46
all_models$model_xgboost_sparse_us_init$cutoff <- 0.46
all_models$model_xgboost_us$cutoff <- 0.46
all_models$model_xgboost_us_init$cutoff <- 0.46

## Modellbewertung
total_eval <- NULL

for(i in 1:length(all_models)){
  model_file <- all_models[[i]]
  model_name <- names(all_models)[i]
  model_type <- define_model_type(model_name)
  
  print(model_name)
  if(model_type%in%c("logit", "xgboost")){
    model_eval <- eval_model(model_file$pred>model_file$cutoff, test_data$success, paste("-", model_file$model_family), model_file$model_type)
  } else if (model_type=="svm"){
    model_eval <- eval_model(model_file$pred, test_data$success, paste("-", model_file$model_family), model_file$model_type)
  }
  all_models[[i]]$model_eval <- model_eval
  
  model_eval_df <- data.frame(list(model_name = model_name, model_eval[1:5]))
  if(i==1){
    total_eval <- model_eval_df
  } else {
    total_eval <- rbind(total_eval, model_eval_df)
  }
}

save(all_models, total_eval, define_model_type, get_predictions, file = "Robjects/models.Rdata")
