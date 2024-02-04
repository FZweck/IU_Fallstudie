rm(list = ls()) 

library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(xgboost)

Sys.setlocale("LC_ALL","de_DE.UTF-8")

load("Robjects/input_data.Rdata")
load("Robjects/models.Rdata")

plot_path <- "C:/Users/feliz/Documents/Master/Kurse/08 Fallstudie/Arbeit/pics/"

## Kosten und Erfolgsrate nach Datum
aggr_data <- final_data %>% 
  left_join(costs) %>% 
  mutate(costs = ifelse(success, cost_success, cost_failure),
         date = round_date(tmsp, unit="day")) 

sum(aggr_data$costs[!aggr_data$success])

aggr_data <- aggr_data %>% 
  group_by(transaction_id, date) %>% 
  summarise(attempts = length(row_id), success = sum(success), amount = unique(amount), costs = sum(costs)) %>% 
  group_by(date) %>% 
  summarise(transactions = length(transaction_id),
            attempts = sum(attempts), success = sum(success), amount = sum(amount), costs = sum(costs)) %>% 
  mutate(success_rate = success / attempts, cost_rate = costs / amount)

sum(aggr_data$costs) / sum(aggr_data$amount)

## Datenszusammenfassung
cols <- c("success", "PSP", "amount", "amount_log", "country", "secured", "card", "attempt")
for (col in cols){
  if(class(final_data[,col])%in%c("logical", "factor")){
    tmp <- data.frame(table(final_data[col]))
    tmp$Perc <- tmp$Freq / sum(tmp$Freq)
    
    out <- paste(col, "&", paste0(gsub("_", "\\_", tmp[,col], fixed = T), " (",  gsub(".", ",", round(tmp$Perc*100, 1), fixed = T), "\\%)", collapse = ", "), "\\\\ \n")
  } else if(class(final_data[,col])=="numeric"){
    out <- paste0(col, " & Minimum: ", min(final_data[col]), ",00; Durchschnitt: ", 
                  gsub(".", ",", round(mean(final_data[,col]),2), fixed = T), "; Maximum: ", 
                  max(final_data[col]), ",00 \\\\ \n")
  } else if(class(final_data[,col])=="integer"){
    out <- paste0(col, " & Minimum: ", min(final_data[col]), "; Durchschnitt: ", 
                  gsub(".", ",", round(mean(final_data[,col]),2), fixed = T), "; Maximum: ", 
                  max(final_data[col]), " \\\\ \n")
  } else {
    stop(error)
  }
  
  cat(out)
}

## EDA
# success nach Zeit
aggr_data <- final_data %>% 
  mutate(date = floor_date(tmsp, unit="day")) %>% 
  group_by(date) %>% 
  summarise(total = length(row_id), success = sum(success)) %>% 
  mutate(success_rate = success / total)

plt <- ggplot(aggr_data, aes(x=date)) +
  geom_line(aes(y=success_rate, color = "Erfolgsrate")) +
  scale_color_brewer(palette="Set2") +
  xlab("Datum") +
  ylab("Erfolgsrate") +
  scale_y_continuous(labels = function(x) paste0(format(x*100, decimal.mark = ","), "%")) +
  theme_light() + 
  theme(legend.position="none")
plt
ggsave(
  filename = paste0(plot_path, "Erfolgsrate01.png"),
  height = 4,
  plot = plt
)



aggr_data <- final_data %>% 
  mutate(date = wday(tmsp, week_start = 1)) %>% 
  group_by(date) %>% 
  summarise(total = length(row_id), success = sum(success)) %>% 
  mutate(success_rate = success / total)

plt <- ggplot(aggr_data, aes(x=date)) +
  geom_line(aes(y=success_rate, color = "Erfolgsrate")) +
  scale_color_brewer(palette="Set2") +
  xlab("Wochentag") +
  ylab("Erfolgsrate") +
  scale_y_continuous(labels = function(x) paste0(format(x*100, decimal.mark = ","), "%")) +
  scale_x_continuous(labels = weekdays(seq(Sys.Date() - wday(Sys.Date()) + 2, Sys.Date() + 8 - wday(Sys.Date()), "day")), breaks = 1:7, minor_breaks = NULL) +
  theme_light() + 
  theme(legend.position="none")
plt
ggsave(
  filename = paste0(plot_path, "Erfolgsrate02.png"),
  height = 4,
  plot = plt
)


aggr_data <- final_data %>% 
  mutate(date = hour(tmsp)) %>% 
  group_by(date) %>% 
  summarise(total = length(row_id), success = sum(success)) %>% 
  mutate(success_rate = success / total)

plt <- ggplot(aggr_data, aes(x=date)) +
  geom_line(aes(y=success_rate, color = "Erfolgsrate")) +
  scale_color_brewer(palette="Set2") +
  xlab("Stunde") +
  ylab("Erfolgsrate") +
  scale_y_continuous(labels = function(x) paste0(format(x*100, decimal.mark = ","), "%")) +
  scale_x_continuous(labels = function(x) formatC(x, width=2, flag="0")) +
  theme_light() + 
  theme(legend.position="none")
plt
ggsave(
  filename = paste0(plot_path, "Erfolgsrate03.png"),
  height = 4,
  plot = plt
)



# Kovariablenanalyse
variables <- c("PSP", "amount", "amount_log", "country", "attempt", "secured", "card")

plot_list <- list()
length(plot_list) <- (length(variables)+1) * (length(variables)+1)
m_pos <- matrix(1:((length(variables)+1) * (length(variables)+1)), ncol = (length(variables)+1))

plot_list[[1]] <- ggplot(final_data, aes(x=1, y=1)) +
  annotate("text", x=1, y=1, label = "") +
  theme_void()

for(i in 1:length(variables)){
  # Labels für Kovariablen
  plt <- ggplot(final_data, aes(x=1, y=1)) +
    annotate("text", x=1, y=1, label = variables[i]) +
    theme_void()
  ggsave(
    filename = paste0(plot_path, "EDA_Kovariablen/Spalte", i, ".png"),
    plot = plt,
    height = 1, width = 1
    )

  ggsave(
    filename = paste0(plot_path, "EDA_Kovariablen/Zeile", i, ".png"),
    plot = plt,
    height = 2.7, width = 1
  )
  
  # Diagnonale
  if(class(final_data[,variables[i]])=="factor" | class(final_data[,variables[i]])=="logical"){
    tmp <- data.frame(table(final_data[,variables[i]]))
    tmp$Perc <- tmp$Freq / sum(tmp$Freq)
    
    plt <- 
      ggplot(tmp, aes(x=Var1, y=Perc, fill=Var1)) +
      geom_bar(stat="identity") +
      geom_text(aes(x=Var1, y=Perc, label=paste0(format(round(Perc*100,1), decimal.mark = ","), "%")), 
                nudge_y = 0.03, size=10) +
      scale_color_brewer(palette="Set2") +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            legend.position="none",
            axis.text.x=element_text(size=20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/Diag", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
  } else {
    plt <- 
      ggplot(final_data, aes(x = final_data[,variables[i]])) + 
      geom_histogram(position = "identity", fill = "firebrick1", color = "firebrick4", alpha = 0.7) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank())
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/Diag", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
  }
  
  if(i!=length(variables)){
    for(j in (i+1):(length(variables))){
      if(class(final_data[,variables[i]])=="factor" | class(final_data[,variables[i]])=="logical"){
        if(class(final_data[,variables[j]])=="factor" | class(final_data[,variables[j]])=="logical"){
          tmp <- data.frame(table(final_data[,variables[i]], final_data[,variables[j]]))
          tmp2 <- data.frame(table(final_data[,variables[i]]))
          
          tmp <- tmp2 %>% 
            mutate(Total = Freq) %>% 
            select(-Freq) %>% 
            left_join(tmp) %>% 
            mutate(Perc = Freq / Total)
          
          plt <- 
            ggplot(tmp, aes(x=Var1, y=Perc)) +
            geom_bar(aes(fill = Var2), position = "dodge", stat = "identity") +
            geom_text(aes(x=Var1, y=Perc+0.015, label=paste0(format(round(Perc*100,1), decimal.mark = ","), "%"), group = Var2), 
                      position = position_dodge2(width = 0.9), size = 5) +
            theme_minimal() +
            theme(axis.title.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size=20),
                  axis.text.y=element_blank(),
                  legend.title = element_blank(),
                  legend.text=element_text(size=20))
          if(variables[i]=="PSP") plt <- plt + scale_x_discrete(guide = guide_axis(n.dodge=2))
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Plot", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
          
          pval <- chisq.test(final_data[,variables[i]], final_data[,variables[j]])$p.value
          if(pval<0.001){
            plevel <- "***"
          } else if(pval<0.01){
            plevel <- "**"
          } else if(pval<0.05){
            plevel <- "*"
          } else if(pval<0.1){
            plevel <- "."
          } else {
            plevel <- ""
          }
          
          plt <- 
            ggplot(final_data, aes(x=1, y=1)) +
            annotate("text", x=1, y=1, label = paste(format(round(pval,5), decimal.mark = ","), plevel), size = 15) +
            theme_void()
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Pval", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
        } else {
          plt <- 
            ggplot(final_data, aes(x=final_data[,variables[i]], y=final_data[,variables[j]], fill = final_data[,variables[i]])) +
            geom_boxplot() +
            coord_flip() +
            theme_minimal() +
            theme(axis.title.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_text(size=20),
                  legend.position="none")
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Plot", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
          
          my_aov <- aov(as.formula(paste(variables[j], variables[i], sep = "~")), data = final_data)
          pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
          if(pval<0.001){
            plevel <- "***"
          } else if(pval<0.01){
            plevel <- "**"
          } else if(pval<0.05){
            plevel <- "*"
          } else if(pval<0.1){
            plevel <- "."
          } else {
            plevel <- ""
          }
          
          plt <- 
            ggplot(final_data, aes(x=1, y=1)) +
            annotate("text", x=1, y=1, label = paste(format(round(pval,5), decimal.mark = ","), plevel), size = 15) +
            theme_void()
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Pval", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
        }
      } else {
        if(class(final_data[,variables[j]])=="factor" | class(final_data[,variables[j]])=="logical"){
          plt <- 
            ggplot(final_data, aes(x=final_data[,variables[j]], y=final_data[,variables[i]], fill = final_data[,variables[j]])) +
            geom_boxplot() +
            theme_minimal() +
            theme(axis.title.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.text.x=element_text(size=20),
                  legend.position="none")
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Plot", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
          
          my_aov <- aov(as.formula(paste(variables[i], variables[j], sep = "~")), data = final_data)
          pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
          if(pval<0.001){
            plevel <- "***"
          } else if(pval<0.01){
            plevel <- "**"
          } else if(pval<0.05){
            plevel <- "*"
          } else if(pval<0.1){
            plevel <- "."
          } else {
            plevel <- ""
          }
          
          plt <- 
            ggplot(final_data, aes(x=1, y=1)) +
            annotate("text", x=1, y=1, label = paste(format(round(pval,5), decimal.mark = ","), plevel), size = 15) +
            theme_void()
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Pval", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
        } else {
          s_random <- sample(1:nrow(final_data), nrow(final_data)*0.1)
          plt <- 
            ggplot(final_data[s_random,], aes(x = final_data[s_random,variables[i]], y = final_data[s_random,variables[j]])) +
            geom_point() +
            theme_minimal() +
            theme(axis.title.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank())
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Plot", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
          
          pval <- cor.test(final_data[,variables[i]],final_data[,variables[j]])$p.value
          if(pval<0.001){
            plevel <- "***"
          } else if(pval<0.01){
            plevel <- "**"
          } else if(pval<0.05){
            plevel <- "*"
          } else if(pval<0.1){
            plevel <- "."
          } else {
            plevel <- ""
          }
          
          plt <- 
            ggplot(final_data, aes(x=1, y=1)) +
            annotate("text", x=1, y=1, label = paste(format(round(pval,5), decimal.mark = ","), plevel), size = 15) +
            theme_void()
          ggsave(
            filename = paste0(plot_path, "EDA_Kovariablen/Pval", i, j, ".png"),
            height = 2.7, width = 7,
            plot = plt
          )
        }
      }
    }
  }
}

# Kovariablen nach Zeit
for(i in 1:length(variables)){
  if(class(final_data[,variables[i]])=="factor" | class(final_data[,variables[i]])=="logical"){
    # Wochentag
    pval <- chisq.test(final_data[,variables[i]], final_data$tmsp_wday)$p.value
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    tmp <- data.frame(table(final_data[,variables[i]], final_data$tmsp_wday))
    tmp2 <- data.frame(table(final_data[,variables[i]]))
    
    tmp <- tmp2 %>% 
      mutate(Total = Freq) %>% 
      select(-Freq) %>% 
      left_join(tmp) %>% 
      mutate(Perc = Freq / Total)
    
    plt <- 
      ggplot(tmp, aes(x=Var1, y=Perc)) +
      geom_bar(aes(fill = Var2), position = "dodge", stat = "identity") +
      geom_text(aes(x=Var1, y=Perc+0.015, label=paste0(format(round(Perc*100,1), decimal.mark = ","), "%"), group = Var2), 
                position = position_dodge2(width = 0.9), size = 5) +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_text(size=20),
            axis.text.y=element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=20),
            plot.title = element_text(size = 20))
    if (variables[i]=="PSP") plt <- plt + scale_x_discrete(guide = guide_axis(n.dodge=2))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/wday", i, ".png"),
      height = 3,
      width = 7,
      plot = plt
    )
    
    # Wochenende / Feiertag
    pval <- chisq.test(final_data[,variables[i]], final_data$tmsp_is_weekend)$p.value
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    tmp <- data.frame(table(final_data[,variables[i]], final_data$tmsp_is_weekend))
    tmp2 <- data.frame(table(final_data[,variables[i]]))
    
    tmp <- tmp2 %>% 
      mutate(Total = Freq) %>% 
      select(-Freq) %>% 
      left_join(tmp) %>% 
      mutate(Perc = Freq / Total)
    
    plt <- 
      ggplot(tmp, aes(x=Var1, y=Perc)) +
      geom_bar(aes(fill = Var2), position = "dodge", stat = "identity") +
      geom_text(aes(x=Var1, y=Perc+0.015, label=paste0(format(round(Perc*100,1), decimal.mark = ","), "%"), group = Var2), 
                position = position_dodge2(width = 0.9), size = 5) +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_text(size=20),
            axis.text.y=element_blank(),
            legend.title = element_blank(),
            legend.text=element_text(size=20),
            plot.title = element_text(size = 20))
    if (variables[i]=="PSP") plt <- plt + scale_x_discrete(guide = guide_axis(n.dodge=2))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/is_weekend", i, ".png"),
      height = 3,
      width = 7,
      plot = plt
    )
    
    # Stunde
    my_aov <- aov(as.formula(paste("tmsp_hour", variables[i], sep = "~")), data = final_data)
    pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    plt <- 
      ggplot(final_data, aes(x=final_data[,variables[i]], y=final_data$tmsp_hour, fill = final_data[,variables[i]])) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/hour", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
    
    # Minute
    my_aov <- aov(as.formula(paste("tmsp_minute", variables[i], sep = "~")), data = final_data)
    pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    plt <- 
      ggplot(final_data, aes(x=final_data[,variables[i]], y=final_data$tmsp_minute, fill = final_data[,variables[i]])) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/minute", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
    
  } else {
    # Wochentag
    my_aov <- aov(as.formula(paste(variables[i], "tmsp_wday", sep = "~")), data = final_data)
    pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    plt <- 
      ggplot(final_data, aes(x=final_data$tmsp_wday, y=final_data[,variables[i]], fill = final_data$tmsp_wday)) +
      geom_boxplot() +
      coord_flip() +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/wday", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
    
    # Wochenende / Feiertag
    my_aov <- aov(as.formula(paste(variables[i], "tmsp_is_weekend", sep = "~")), data = final_data)
    pval <- summary(my_aov)[[1]][["Pr(>F)"]][1]
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    plt <- 
      ggplot(final_data, aes(x=final_data$tmsp_is_weekend, y=final_data[,variables[i]], fill = final_data$tmsp_is_weekend)) +
      geom_boxplot() +
      coord_flip() +
      theme_minimal() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/is_weekend", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
    
    # Stunde
    pval <- cor.test(final_data[,variables[i]], final_data$tmsp_hour)$p.value
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    s_random <- sample(1:nrow(final_data), nrow(final_data)*0.25)
    plt <- 
      ggplot(final_data[s_random,], aes(x=final_data[s_random,variables[i]], y=final_data$tmsp_hour[s_random])) +
      geom_point() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/hour", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
    
    # Minute
    pval <- cor.test(final_data[,variables[i]], final_data$tmsp_minute)$p.value
    if(pval<0.001){
      plevel <- "***"
    } else if(pval<0.01){
      plevel <- "**"
    } else if(pval<0.05){
      plevel <- "*"
    } else if(pval<0.1){
      plevel <- "."
    } else {
      plevel <- ""
    }
    
    s_random <- sample(1:nrow(final_data), nrow(final_data)*0.25)
    plt <- 
      ggplot(final_data[s_random,], aes(x=final_data[s_random,variables[i]], y=final_data$tmsp_minute[s_random])) +
      geom_point() +
      labs(title = paste0("P-Wert: ", format(round(pval,5), decimal.mark = ","), plevel)) +
      theme_minimal() +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_text(size=20),
            legend.position="none",
            plot.title = element_text(size = 20))
    ggsave(
      filename = paste0(plot_path, "EDA_Kovariablen/minute", i, ".png"),
      height = 2.7, width = 7,
      plot = plt
    )
  }
}

## Feature-Importance
my_model <- all_models$model_xgboost$model
importance_matrix <- xgb.importance(model = my_model)
importance_matrix <- xgb.plot.importance(importance_matrix)
importance_matrix <- importance_matrix[order(importance_matrix$Importance, decreasing  = T),]

plt <- 
  ggplot(importance_matrix, aes(x = reorder(Feature, desc(-Importance)), y=Importance)) +
  geom_bar(stat="identity", fill = "aquamarine3") +
  geom_text(aes(x=reorder(Feature, desc(-Importance)), y=Importance, label=paste0(format(round(Importance*100,2), decimal.mark = ","), "%")), 
            nudge_y = 0.015, size=4) +
  coord_flip() + 
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10),
        axis.text.x=element_blank(),
        legend.position="none")

ggsave(
      filename = paste0(plot_path, "FeatureImportance.png"),
      height = 3.5,
      plot = plt
    )


## Modellbewertung
params <- list(
  # LogReg Gesamt
  logit_all_cutoff = paste(all_models$model_logit$cutoff*100, "\\%"),
  logit_all_accuracy = format(round(all_models$model_logit$model_eval$accuracy, 4), decimal.mark = ","),
  logit_all_auc = format(round(all_models$model_logit$model_eval$auc, 4), decimal.mark = ","),
  logit_all_f = format(round(all_models$model_logit$model_eval$F1, 4), decimal.mark = ","),
  logit_all_recall = format(round(all_models$model_logit$model_eval$recall, 4), decimal.mark = ","),
  # LogReg Undersampling
  logit_us_cutoff = paste(all_models$model_logit_us$cutoff*100, "\\%"),
  logit_us_accuracy = format(round(all_models$model_logit_us$model_eval$accuracy, 4), decimal.mark = ","),
  logit_us_auc = format(round(all_models$model_logit_us$model_eval$auc, 4), decimal.mark = ","),
  logit_us_f = format(round(all_models$model_logit_us$model_eval$F1, 4), decimal.mark = ","),
  logit_us_recall = format(round(all_models$model_logit_us$model_eval$recall, 4), decimal.mark = ","),
  # LogReg Gesamt sparse
  logit_sparse_all_cutoff = paste(all_models$model_logit_sparse$cutoff*100, "\\%"),
  logit_sparse_all_accuracy = format(round(all_models$model_logit_sparse$model_eval$accuracy, 4), decimal.mark = ","),
  logit_sparse_all_auc = format(round(all_models$model_logit_sparse$model_eval$auc, 4), decimal.mark = ","),
  logit_sparse_all_f = format(round(all_models$model_logit_sparse$model_eval$F1, 4), decimal.mark = ","),
  logit_sparse_all_recall = format(round(all_models$model_logit_sparse$model_eval$recall, 4), decimal.mark = ","),
  # LogReg Undersampling sparse
  logit_sparse_us_cutoff = paste(all_models$model_logit_us_sparse$cutoff*100, "\\%"),
  logit_sparse_us_accuracy = format(round(all_models$model_logit_us_sparse$model_eval$accuracy, 4), decimal.mark = ","),
  logit_sparse_us_auc = format(round(all_models$model_logit_us_sparse$model_eval$auc, 4), decimal.mark = ","),
  logit_sparse_us_f = format(round(all_models$model_logit_us_sparse$model_eval$F1, 4), decimal.mark = ","),
  logit_sparse_us_recall = format(round(all_models$model_logit_sparse$model_eval$recall, 4), decimal.mark = ","),
  # SVM Init
  svm_init_cutoff = "-",
  svm_init_accuracy = format(round(all_models$model_svm_init$model_eval$accuracy, 4), decimal.mark = ","),
  svm_init_auc = format(round(all_models$model_svm_init$model_eval$auc, 4), decimal.mark = ","),
  svm_init_f = format(round(all_models$model_svm_init$model_eval$F1, 4), decimal.mark = ","),
  svm_init_gamma = format(round(all_models$model_svm_init$model$gamma,4), decimal.mark = ","),
  svm_init_cost = format(all_models$model_svm_init$model$cost, decimal.mark = ","),
  # SVM Optimiert
  svm_opt_cutoff = "-",
  svm_opt_accuracy = format(round(all_models$model_svm$model_eval$accuracy, 4), decimal.mark = ","),
  svm_opt_auc = format(round(all_models$model_svm$model_eval$auc, 4), decimal.mark = ","),
  svm_opt_f = format(round(all_models$model_svm$model_eval$F1, 4), decimal.mark = ","),
  svm_opt_gamma = format(all_models$model_svm$model$gamma, decimal.mark = ","),
  svm_opt_cost = format(all_models$model_svm$model$cost, decimal.mark = ","),
  # SVM sparse Init
  svm_sparse_init_cutoff = "-",
  svm_sparse_init_accuracy = format(round(all_models$model_svm_sparse_init$model_eval$accuracy, 4), decimal.mark = ","),
  svm_sparse_init_auc = format(round(all_models$model_svm_sparse_init$model_eval$auc, 4), decimal.mark = ","),
  svm_sparse_init_f = format(round(all_models$model_svm_sparse_init$model_eval$F1, 4), decimal.mark = ","),
  svm_sparse_init_gamma = format(round(all_models$model_svm_sparse_init$model$gamma,4), decimal.mark = ","),
  svm_sparse_init_cost = format(all_models$model_svm_sparse_init$model$cost, decimal.mark = ","),
  # SVM sparse Optimiert
  svm_sparse_opt_cutoff = "-",
  svm_sparse_opt_accuracy = format(round(all_models$model_svm_sparse$model_eval$accuracy, 4), decimal.mark = ","),
  svm_sparse_opt_auc = format(round(all_models$model_svm_sparse$model_eval$auc, 4), decimal.mark = ","),
  svm_sparse_opt_f = format(round(all_models$model_svm_sparse$model_eval$F1, 4), decimal.mark = ","),
  svm_sparse_opt_gamma = format(all_models$model_svm_sparse$model$gamma, decimal.mark = ","),
  svm_sparse_opt_cost = format(all_models$model_svm_sparse$model$cost, decimal.mark = ","),
  # XGBoost all Init
  xgboost_all_init_cutoff = paste(all_models$model_xgboost_init$cutoff*100, "\\%"),
  xgboost_all_init_accuracy = format(round(all_models$model_xgboost_init$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_all_init_auc = format(round(all_models$model_xgboost_init$model_eval$auc, 4), decimal.mark = ","),
  xgboost_all_init_f = format(round(all_models$model_xgboost_init$model_eval$F1, 4), decimal.mark = ","),
  xgboost_all_init_eta = ifelse(is.null(all_models$model_xgboost_init$model$params$eta), "0,3", format(all_models$model_xgboost_init$model$params$eta, decimal.mark = ",")),
  xgboost_all_init_depth = ifelse(is.null(all_models$model_xgboost_init$model$params$max_depth), "6", format(all_models$model_xgboost_init$model$params$max_depth, decimal.mark = ",")),
  xgboost_all_init_child = ifelse(is.null(all_models$model_xgboost_init$model$params$min_child_weight), "1", format(all_models$model_xgboost_init$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_all_init_gamma = ifelse(is.null(all_models$model_xgboost_init$model$params$gamma), "0", format(all_models$model_xgboost_init$model$params$gamma, decimal.mark = ",")),
  xgboost_all_init_colsample = ifelse(is.null(all_models$model_xgboost_init$model$params$colsample_bytree), "1", format(all_models$model_xgboost_init$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost all opt
  xgboost_all_opt_cutoff = paste(all_models$model_xgboost$cutoff*100, "\\%"),
  xgboost_all_opt_accuracy = format(round(all_models$model_xgboost$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_all_opt_auc = format(round(all_models$model_xgboost$model_eval$auc, 4), decimal.mark = ","),
  xgboost_all_opt_f = format(round(all_models$model_xgboost$model_eval$F1, 4), decimal.mark = ","),
  xgboost_all_opt_eta = ifelse(is.null(all_models$model_xgboost$model$params$eta), "0,3", format(all_models$model_xgboost$model$params$eta, decimal.mark = ",")),
  xgboost_all_opt_depth = ifelse(is.null(all_models$model_xgboost$model$params$max_depth), "6", format(all_models$model_xgboost$model$params$max_depth, decimal.mark = ",")),
  xgboost_all_opt_child = ifelse(is.null(all_models$model_xgboost$model$params$min_child_weight), "1", format(all_models$model_xgboost$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_all_opt_gamma = ifelse(is.null(all_models$model_xgboost$model$params$gamma), "0", format(all_models$model_xgboost$model$params$gamma, decimal.mark = ",")),
  xgboost_all_opt_colsample = ifelse(is.null(all_models$model_xgboost$model$params$colsample_bytree), "1", format(all_models$model_xgboost$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost Undersampling init
  xgboost_us_init_cutoff = paste(all_models$model_xgboost_us_init$cutoff*100, "\\%"),
  xgboost_us_init_accuracy = format(round(all_models$model_xgboost_us_init$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_us_init_auc = format(round(all_models$model_xgboost_us_init$model_eval$auc, 4), decimal.mark = ","),
  xgboost_us_init_f = format(round(all_models$model_xgboost_us_init$model_eval$F1, 4), decimal.mark = ","),
  xgboost_us_init_eta = ifelse(is.null(all_models$model_xgboost_us_init$model$params$eta), "0,3", format(all_models$model_xgboost_us_init$model$params$eta, decimal.mark = ",")),
  xgboost_us_init_depth = ifelse(is.null(all_models$model_xgboost_us_init$model$params$max_depth), "6", format(all_models$model_xgboost_us_init$model$params$max_depth, decimal.mark = ",")),
  xgboost_us_init_child = ifelse(is.null(all_models$model_xgboost_us_init$model$params$min_child_weight), "1", format(all_models$model_xgboost_us_init$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_us_init_gamma = ifelse(is.null(all_models$model_xgboost_us_init$model$params$gamma), "0", format(all_models$model_xgboost_us_init$model$params$gamma, decimal.mark = ",")),
  xgboost_us_init_colsample = ifelse(is.null(all_models$model_xgboost_us_init$model$params$colsample_bytree), "1", format(all_models$model_xgboost_us_init$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost Undersampling opt
  xgboost_us_opt_cutoff = paste(all_models$model_xgboost_us$cutoff*100, "\\%"),
  xgboost_us_opt_accuracy = format(round(all_models$model_xgboost_us$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_us_opt_auc = format(round(all_models$model_xgboost_us$model_eval$auc, 4), decimal.mark = ","),
  xgboost_us_opt_f = format(round(all_models$model_xgboost_us$model_eval$F1, 4), decimal.mark = ","),
  xgboost_us_opt_eta = ifelse(is.null(all_models$model_xgboost_us$model$params$eta), "0,3", format(all_models$model_xgboost_us$model$params$eta, decimal.mark = ",")),
  xgboost_us_opt_depth = ifelse(is.null(all_models$model_xgboost_us$model$params$max_depth), "6", format(all_models$model_xgboost_us$model$params$max_depth, decimal.mark = ",")),
  xgboost_us_opt_child = ifelse(is.null(all_models$model_xgboost_us$model$params$min_child_weight), "1", format(all_models$model_xgboost_us$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_us_opt_gamma = ifelse(is.null(all_models$model_xgboost_us$model$params$gamma), "0", format(all_models$model_xgboost_us$model$params$gamma, decimal.mark = ",")),
  xgboost_us_opt_colsample = ifelse(is.null(all_models$model_xgboost_us$model$params$colsample_bytree), "1", format(all_models$model_xgboost_us$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost all sparse Init
  xgboost_all_sparse_init_cutoff = paste(all_models$model_xgboost_sparse_init$cutoff*100, "\\%"),
  xgboost_all_sparse_init_accuracy = format(round(all_models$model_xgboost_sparse_init$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_all_sparse_init_auc = format(round(all_models$model_xgboost_sparse_init$model_eval$auc, 4), decimal.mark = ","),
  xgboost_all_sparse_init_f = format(round(all_models$model_xgboost_sparse_init$model_eval$F1, 4), decimal.mark = ","),
  xgboost_all_sparse_init_eta = ifelse(is.null(all_models$model_xgboost_sparse_init$model$params$eta), "0,3", format(all_models$model_xgboost_sparse_init$model$params$eta, decimal.mark = ",")),
  xgboost_all_sparse_init_depth = ifelse(is.null(all_models$model_xgboost_sparse_init$model$params$max_depth), "6", format(all_models$model_xgboost_sparse_init$model$params$max_depth, decimal.mark = ",")),
  xgboost_all_sparse_init_child = ifelse(is.null(all_models$model_xgboost_sparse_init$model$params$min_child_weight), "1", format(all_models$model_xgboost_sparse_init$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_all_sparse_init_gamma = ifelse(is.null(all_models$model_xgboost_sparse_init$model$params$gamma), "0", format(all_models$model_xgboost_sparse_init$model$params$gamma, decimal.mark = ",")),
  xgboost_all_sparse_init_colsample = ifelse(is.null(all_models$model_xgboost_sparse_init$model$params$colsample_bytree), "1", format(all_models$model_xgboost_sparse_init$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost all sparse opt
  xgboost_all_sparse_opt_cutoff = paste(all_models$model_xgboost_sparse$cutoff*100, "\\%"),
  xgboost_all_sparse_opt_accuracy = format(round(all_models$model_xgboost_sparse$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_all_sparse_opt_auc = format(round(all_models$model_xgboost_sparse$model_eval$auc, 4), decimal.mark = ","),
  xgboost_all_sparse_opt_f = format(round(all_models$model_xgboost_sparse$model_eval$F1, 4), decimal.mark = ","),
  xgboost_all_sparse_opt_eta = ifelse(is.null(all_models$model_xgboost_sparse$model$params$eta), "0,3", format(all_models$model_xgboost_sparse$model$params$eta, decimal.mark = ",")),
  xgboost_all_sparse_opt_depth = ifelse(is.null(all_models$model_xgboost_sparse$model$params$max_depth), "6", format(all_models$model_xgboost_sparse$model$params$max_depth, decimal.mark = ",")),
  xgboost_all_sparse_opt_child = ifelse(is.null(all_models$model_xgboost_sparse$model$params$min_child_weight), "1", format(all_models$model_xgboost_sparse$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_all_sparse_opt_gamma = ifelse(is.null(all_models$model_xgboost_sparse$model$params$gamma), "0", format(all_models$model_xgboost_sparse$model$params$gamma, decimal.mark = ",")),
  xgboost_all_sparse_opt_colsample = ifelse(is.null(all_models$model_xgboost_sparse$model$params$colsample_bytree), "1", format(all_models$model_xgboost_sparse$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost Undersampling sparse init
  xgboost_us_sparse_init_cutoff = paste(all_models$model_xgboost_sparse_us_init$cutoff*100, "\\%"),
  xgboost_us_sparse_init_accuracy = format(round(all_models$model_xgboost_sparse_us_init$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_us_sparse_init_auc = format(round(all_models$model_xgboost_sparse_us_init$model_eval$auc, 4), decimal.mark = ","),
  xgboost_us_sparse_init_f = format(round(all_models$model_xgboost_sparse_us_init$model_eval$F1, 4), decimal.mark = ","),
  xgboost_us_sparse_init_eta = ifelse(is.null(all_models$model_xgboost_sparse_us_init$model$params$eta), "0,3", format(all_models$model_xgboost_sparse_us_init$model$params$eta, decimal.mark = ",")),
  xgboost_us_sparse_init_depth = ifelse(is.null(all_models$model_xgboost_sparse_us_init$model$params$max_depth), "6", format(all_models$model_xgboost_sparse_us_init$model$params$max_depth, decimal.mark = ",")),
  xgboost_us_sparse_init_child = ifelse(is.null(all_models$model_xgboost_sparse_us_init$model$params$min_child_weight), "1", format(all_models$model_xgboost_sparse_us_init$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_us_sparse_init_gamma = ifelse(is.null(all_models$model_xgboost_sparse_us_init$model$params$gamma), "0", format(all_models$model_xgboost_sparse_us_init$model$params$gamma, decimal.mark = ",")),
  xgboost_us_sparse_init_colsample = ifelse(is.null(all_models$model_xgboost_sparse_us_init$model$params$colsample_bytree), "1", format(all_models$model_xgboost_sparse_us_init$model$params$colsample_bytree, decimal.mark = ",")),
  # XGBoost Undersampling sparse opt
  xgboost_us_sparse_opt_cutoff = paste(all_models$model_xgboost_sparse_us$cutoff*100, "\\%"),
  xgboost_us_sparse_opt_accuracy = format(round(all_models$model_xgboost_sparse_us$model_eval$accuracy, 4), decimal.mark = ","),
  xgboost_us_sparse_opt_auc = format(round(all_models$model_xgboost_sparse_us$model_eval$auc, 4), decimal.mark = ","),
  xgboost_us_sparse_opt_f = format(round(all_models$model_xgboost_sparse_us$model_eval$F1, 4), decimal.mark = ","),
  xgboost_us_sparse_opt_eta = ifelse(is.null(all_models$model_xgboost_sparse_us$model$params$eta), "0,3", format(all_models$model_xgboost_sparse_us$model$params$eta, decimal.mark = ",")),
  xgboost_us_sparse_opt_depth = ifelse(is.null(all_models$model_xgboost_sparse_us$model$params$max_depth), "6", format(all_models$model_xgboost_sparse_us$model$params$max_depth, decimal.mark = ",")),
  xgboost_us_sparse_opt_child = ifelse(is.null(all_models$model_xgboost_sparse_us$model$params$min_child_weight), "1", format(all_models$model_xgboost_sparse_us$model$params$min_child_weight, decimal.mark = ",")),
  xgboost_us_sparse_opt_gamma = ifelse(is.null(all_models$model_xgboost_sparse_us$model$params$gamma), "0", format(all_models$model_xgboost_sparse_us$model$params$gamma, decimal.mark = ",")),
  xgboost_us_sparse_opt_colsample = ifelse(is.null(all_models$model_xgboost_sparse_us$model$params$colsample_bytree), "1", format(all_models$model_xgboost_sparse_us$model$params$colsample_bytree, decimal.mark = ","))
  )
  
for(i in 1:length(params)){
  my_str <- gsub("_", "", paste0("\\newcommand\\", names(params)[i], "{", params[[i]], "}\n"))
  cat(my_str)
}
