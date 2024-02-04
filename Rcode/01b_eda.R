rm(list = ls())

# Laden relevanter R packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(ggpubr)
library(EnvStats)

# Globale Objekte
load("Robjects/input_data.Rdata")

###################################
## explorative Datenanalyse #######
###################################

## Hilfsfunktionen ####
# Analyse einzelner Variablen
factor_analysis <- function(x, main = NULL, sub = NULL, output = NULL){
  # Erstellen der Häufigkeitstabelle inkl. Prozentsatz
  tmp <- data.frame(table(x))
  tmp$Perc <- tmp$Freq / sum(tmp$Freq)
  
  # Ausgabe der Häufigkeitstabelle in der Konsole
  if(is.null(output)) print(tmp)
  
  # Ausgabe eines Pie-Charts, inkl. eines Chart-Titels, falls gewünscht
  pie_chart <- ggplot(tmp, aes(x="", y=Perc, fill=x)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    geom_text(aes(label = paste0(x, " (", round(Perc*100,2), "%)")),
              position = position_stack(vjust = 0.5)) + 
    labs(title = main, subtitle = sub) +
    labs(x="", y = "") +
    theme(panel.background = element_rect(fill = 'white', color = 'white'),
          panel.grid=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
  if(is.null(output)) print(pie_chart)
  
  if(!is.null(output)){
    if(output=="plot"){
      plt <- ggplot(tmp, aes(x=x, y=Perc, fill=x)) +
        geom_bar(stat="identity") + 
        labs(title = main, subtitle = sub)
      return(plt)
    }
  }
}

numerical_analysis <- function(x, main = NULL, sub = NULL, output = NULL){
  # Zusammenfassung
  if(is.null(output)) print(summary(x))
  
  # Histogramm
  df <- data.frame(x)
  plt <- ggplot(df, aes(x = x)) + 
    geom_histogram(position = "identity", fill = "firebrick1", color = "firebrick4", alpha = 0.5) + 
    labs(title = main, subtitle = sub)
  if(is.null(output)) print(plt)
  
  if(!is.null(output)){
    if(output=="plot") return(plt)
  }
}

# Zeitverläufe
plot_success_by_time <- function(df, unit, include_absolute=T, main = NULL, sub = NULL){
  if(unit=="weekday"){
    aggr_data <- df %>% 
      mutate(date = wday(tmsp, week_start = 1)) %>% 
      group_by(date) %>% 
      summarise(total = length(row_id), success = sum(success)) %>% 
      mutate(success_rate = success / total)
  } else if(unit=="day"){
    aggr_data <- df %>% 
      mutate(date = round_date(tmsp, unit=unit)) %>% 
      group_by(date) %>% 
      summarise(total = length(row_id), success = sum(success)) %>% 
      mutate(success_rate = success / total)
  } else if(unit=="hour"){
    aggr_data <- df %>% 
      mutate(date = hour(tmsp)) %>% 
      group_by(date) %>% 
      summarise(total = length(row_id), success = sum(success)) %>% 
      mutate(success_rate = success / total)
  } else if(unit=="minute"){
    aggr_data <- df %>% 
      mutate(date = minute(tmsp)) %>% 
      group_by(date) %>% 
      summarise(total = length(row_id), success = sum(success)) %>% 
      mutate(success_rate = success / total)
  } else {
    error("Unknow unit.")
  }
  
  max_total <- max(aggr_data$total)
  if(include_absolute){
    plt <- ggplot(aggr_data, aes(x=date)) +
      geom_line(aes(y=total, color = " Total")) +
      geom_line(aes(y=success, color = "Erfolge")) + 
      geom_line(aes(y=success_rate*max(total), color = "Erfolgsrate"), linetype = "dashed") +
      scale_y_continuous(sec.axis = sec_axis(trans = ~./max_total, labels = percent, 
                                             name = "Erfolgsrate")) + 
      labs(title = main, subtitle = sub)
  } else {
    plt <- ggplot(aggr_data, aes(x=date)) +
      geom_line(aes(y=success_rate*max(total), color = "Erfolgsrate")) + 
      labs(title = main, subtitle = sub)
  }
  
  print(plt)
}

plot_success_by_time_and_category <- function(success, cat, tmsp, main = NULL, sub = NULL){
  df <- data.frame(success, cat, tmsp) %>% 
    group_by(date=round_date(tmsp, "day"), cat) %>% 
    summarise(success_rate = mean(success))
  
  plt <- ggplot(df, aes(x=date, y=success_rate, group=cat, color=cat)) +
    geom_line() + 
    labs(title = main, subtitle = sub)
  print(plt)
}

# Vergleich zweier Variablen
compare_factors <- function(x, y, main = NULL, sub = NULL, output = NULL){
  # Kreuztabelle
  df = data.frame(x, y)
  if(is.null(output)) print(xtabs(~x+y, data = df))
  
  # Plot
  aggr_data <- df %>% 
    group_by(x) %>% 
    summarise(success_rate = mean(y))
  plt <- ggplot(aggr_data, aes(x=x, y=success_rate, fill=x)) +
    geom_bar(stat="identity") + 
    labs(title = main, subtitle = sub)
  if(is.null(output)) print(plt)
  
  if(is.null(output)) print(chisq.test(x, y))
  
  if(!is.null(output)){
    if(output=="plot"){
      aggr_data <- df %>% 
        group_by(x, y) %>% 
        summarise(value = length(x))
      plt <- ggplot(aggr_data, aes(x=x, y=value)) +
        geom_bar(aes(fill = y), position = "dodge", stat = "identity") + 
        labs(title = main, subtitle = sub)
      return(plt)
    } else if(output=="test") {
      res <- paste("P-value:", round(chisq.test(x, y)$p.value, 5))
      return(res)
    }
  }
}

compare_factor_numerical <- function(factor, numerical, main = NULL, sub = NULL, output = NULL){
  df <- data.frame(factor, numerical)
  
  # One-way-Anova
  my_aov <- aov(numerical~factor, data = df)
  if(is.null(output)) print(summary(my_aov))
  
  # Boxplot
  plt <- ggplot(df, aes(x=factor, y=numerical, fill = factor)) +
    geom_boxplot() + 
    labs(title = main, subtitle = sub)
  if(is.null(output)) print(plt)
  
  if(!is.null(output)){
    if(output=="plot"){
      return(plt)
    } else if(output=="test") {
      res <- paste("P-value:", round(summary(my_aov)[[1]][["Pr(>F)"]][1], 5))
      return(res)
    }
  }
}

compare_numericals <- function(x, y, main = NULL, sub = NULL, output = NULL){
  df <- data.frame(x, y)
  
  # Plot
  plt <- ggplot(df, aes(x = x, y = y)) +
    geom_point() + 
    labs(title = main, subtitle = sub)
  
  # Kovarianz und Test
  cor <- cor(x,y)
  p_value <- cor.test(x,y)$p.value
  
  if(!is.null(output)){
    if(output=="plot"){
      return(plt)
    } else if(output=="test") {
      res <- paste("Correlation:", round(cor, 5), "(P-value:", round(p_value, 5), ")")
      return(res)
    }
  }
}

# Analyse der Ko-Variablen
analyse_covariates <- function(df, type = "single", plot=T){
  
  plot_list <- list()
  
  if(type=="single"){
    # Analyse der einzelnen Kovariablen
    for(i in 1:ncol(df)){
      if(class(df[,i])=="factor" | class(df[,i])=="logical"){
        plt <- factor_analysis(df[,i], 
                               main = names(df)[i],
                               output = "plot")
      } else {
        plt <- numerical_analysis(df[,i], 
                                  main = names(df)[i],
                                  output = "plot")
      }
      plot_list[[length(plot_list)+1]] <- plt
    }
  } else if(type=="compare"){
    # Vergleich Kovariablen
    for(i in 1:(ncol(df)-1)){
      for(j in (i+1):ncol(df)){
        if(class(df[,i])=="factor" | class(df[,i])=="logical"){
          if(class(df[,j])=="factor" | class(df[,j])=="logical"){
            test <- compare_factors(df[,i], df[,j], 
                                    main = paste(names(df)[i], "vs.", names(df)[j]),
                                    output = "test")
            plt <- compare_factors(df[,i], df[,j], 
                                   main = paste(names(df)[i], "vs.", names(df)[j]),
                                   sub = test,
                                   output = "plot")
          } else {
            test <- compare_factor_numerical(df[,i], df[,j], 
                                             main = paste(names(df)[i], "vs.", names(df)[j]),
                                             output = "test")
            plt <- compare_factor_numerical(df[,i], df[,j], 
                                            main = paste(names(df)[i], "vs.", names(df)[j]),
                                            sub = test,
                                            output = "plot")
            
          }
        } else {
          if(class(df[,j])=="factor" | class(df[,j])=="logical"){
            test <- compare_factor_numerical(df[,j], df[,i], 
                                             main = paste(names(df)[i], "vs.", names(df)[j]),
                                             output = "test")
            plt <- compare_factor_numerical(df[,j], df[,i], 
                                            main = paste(names(df)[i], "vs.", names(df)[j]),
                                            sub = test,
                                            output = "plot")
          } else {
            test <- compare_numericals(df[,i], df[,j], 
                                       main = paste(names(df)[i], "vs.", names(df)[j]),
                                       output = "test")
            plt <- compare_numericals(df[,i], df[,j], 
                                      main = paste(names(df)[i], "vs.", names(df)[j]),
                                      sub = test,
                                      output = "plot")
          }
        }
        
        print(paste(names(df)[i], "vs.", names(df)[j], ":", test))
        plot_list[[length(plot_list)+1]] <- plt
      }
    }
  }
  
  # Ausgabe der Ergebnisse
  if(plot){
    if(length(plot_list)>20){
      for(i in 0:floor(length(plot_list)/20)){
        sub_list = plot_list[(i*20+1):min(((i+1)*20), length(plot_list))]
        print(ggarrange(plotlist = sub_list))
      }
    } else {
      print(ggarrange(plotlist = plot_list))
    }
  }
}



## Zusammenfassung des Datensatzes ####
summary(final_data)

## Erste Analyse der Zielvariablen ####
# Faktor Analyse
factor_analysis(final_data$success,
                main = "Tortendiagram für success")

# Zeitverlauf
plot_success_by_time(final_data, unit="day")
plot_success_by_time(final_data, unit="weekday")
plot_success_by_time(final_data, unit="hour", include_absolute = F)
plot_success_by_time(final_data, unit="minute")

## Erste Analyse der Input-Variablen ####
# Zahlungsdienstleister
factor_analysis(final_data$PSP)
compare_factors(final_data$PSP, final_data$success)
plot_success_by_time_and_category(final_data$success, final_data$PSP, final_data$tmsp)

# Überweisungsbetrag
numerical_analysis(final_data$amount)
compare_factor_numerical(final_data$success, final_data$amount)

numerical_analysis(final_data$amount_log)
compare_factor_numerical(final_data$success, final_data$amount_log)

# Herkunftsland
factor_analysis(final_data$country)
compare_factors(final_data$country, final_data$success)

# Anzahl der Versuche
numerical_analysis(final_data$attempt)
compare_factor_numerical(final_data$success, final_data$attempt)

# 3D-Identifizierung
factor_analysis(final_data$secured)
compare_factors(final_data$secured, final_data$success)

# Kartenanbieter
factor_analysis(final_data$card)
compare_factors(final_data$card, final_data$success)

# Zeitvariablen
compare_factors(final_data$tmsp_wday, final_data$success)
compare_factors(final_data$tmsp_is_weekend, final_data$success)
compare_factor_numerical(final_data$success, final_data$tmsp_hour)
compare_factor_numerical(final_data$success, final_data$tmsp_minute)

## Vergleich der Kovariablen
covariates <- final_data %>% 
  select(PSP, amount, amount_log, country, attempt, secured, card,
         tmsp_wday, tmsp_is_weekend, tmsp_hour, tmsp_minute)

analyse_covariates(covariates, "single")
analyse_covariates(covariates, "compare")


analyse_covariates(covariates, "compare", plot=F)

## Ausreißer-Analyse amount & amount_log
boxplot(final_data$amount)
length(boxplot(final_data$amount)$out)
rosnerTest(final_data$amount, k = 150)

boxplot(final_data$amount_log)
length(boxplot(final_data$amount_log)$out)
rosnerTest(final_data$amount_log, k = 2900)
# -> keine Anzeichen für Ausreißer

